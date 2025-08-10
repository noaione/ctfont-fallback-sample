use std::{ptr::NonNull, sync::Arc};

use objc2_core_foundation::{
    CFArray, CFDictionary, CFNumber, CFRange, CFRetained, CFString, CFType, CFURL,
    CGAffineTransform, Type,
};
use objc2_core_text::{
    CTFont, CTFontCollection, CTFontDescriptor, CTFontManagerCreateFontDescriptorsFromURL,
    kCTFontFamilyNameAttribute, kCTFontSlantTrait, kCTFontTraitsAttribute, kCTFontURLAttribute,
    kCTFontVariationAxisMaximumValueKey, kCTFontVariationAxisMinimumValueKey,
    kCTFontVariationAxisNameKey, kCTFontWeightTrait,
};
use sbr_util::math::I16Dot16;
use thiserror::Error;

use crate::simul::{FaceInfo, FontAxisValues, FontSource, PlatformFontProvider};

fn codepoint_to_utf16(mut value: u32) -> ([u16; 2], bool) {
    if value < 0x10000 {
        ([value as u16, 0], false)
    } else {
        value -= 0x10000;
        (
            [
                (((value & 0b1111_1111_1100_0000_0000) >> 10) + 0xD800) as u16,
                ((value & 0b0000_0000_0011_1111_1111) + 0xDC00) as u16,
            ],
            true,
        )
    }
}

fn deduplicate_fonts_results(fonts: Vec<FaceInfo>) -> Vec<FaceInfo> {
    // we don't want to include family names that are already in the list
    // and have the same width/weight/italic (we ignore source for now)
    let mut seen = std::collections::HashSet::new();
    fonts
        .into_iter()
        .filter(|font| {
            let family_names = font.family_names.join(",");
            let width = match &font.width {
                FontAxisValues::Fixed(value) => value.to_string(),
                FontAxisValues::Range(min, max) => format!("{}-{}", min, max),
            };
            let weight = match &font.weight {
                FontAxisValues::Fixed(value) => value.to_string(),
                FontAxisValues::Range(min, max) => format!("{}-{}", min, max),
            };
            let italic = font.italic.to_string();
            let key = format!("{}|{}|{}|{}", family_names, width, weight, italic);

            seen.insert(key)
        })
        .collect()
}

#[derive(Debug, Error)]
pub enum NewError {
    #[error("Failed to map font descriptor to FaceInfo")]
    MappingError,
}

#[derive(Debug)]
pub struct CoreTextFontProvider {
    /// Collected fonts at startup.
    fonts: Vec<FaceInfo>,
}

fn normalized_weight_to_css_weight(weight: f64) -> i32 {
    // Scale the f64 to an integer for matching.
    let scaled_weight = (weight * 100.0).round() as i32;

    match scaled_weight {
        80..=100 => 900,
        60..=79 => 800,
        40..=59 => 700,
        20..=39 => 600,
        1..=19 => 500,
        -19..=0 => 400,
        -39..=-20 => 300,
        -59..=-40 => 200,
        -79..=-60 => 100,
        _ => 400, // Default to normal weight if out of range
    }
}

fn css_weight_to_normalized(weight: i32) -> f64 {
    // Convert CSS weight to normalized weight
    match weight {
        100 => -0.8,
        200 => -0.6,
        300 => -0.4,
        400 => 0.0,
        500 => 0.23,
        600 => 0.3,
        700 => 0.4,
        800 => 0.6,
        900 => 0.8,
        _ => 0.0, // Default to normal weight if out of range
    }
}

impl CoreTextFontProvider {
    pub fn new() -> Self {
        let font_collection = unsafe { CTFontCollection::from_available_fonts(None) };

        let descriptors = unsafe { font_collection.matching_font_descriptors() };
        let font_collected = if let Some(descriptors) = descriptors {
            // map CFRetained<CFArray> to
            let descriptors =
                unsafe { CFRetained::cast_unchecked::<CFArray<CTFontDescriptor>>(descriptors) };

            descriptors
                .iter()
                .filter_map(|descriptor| {
                    // Convert the descriptor to a FaceInfo
                    Self::font_from_descriptor(descriptor, None)
                })
                .collect::<Vec<FaceInfo>>()
        } else {
            Vec::new()
        };

        Self {
            fonts: deduplicate_fonts_results(font_collected),
        }
    }

    fn font_to_variable_axes(
        font: &CFRetained<CTFont>,
    ) -> Option<CFRetained<CFArray<CFDictionary<CFString, CFType>>>> {
        let variation_axes = unsafe { font.variation_axes() };
        if let Some(axes) = variation_axes {
            unsafe {
                Some(CFRetained::cast_unchecked::<
                    CFArray<CFDictionary<CFString, CFType>>,
                >(axes))
            }
        } else {
            None
        }
    }

    fn descriptor_to_variable_axes(
        descriptor: CFRetained<CTFontDescriptor>,
    ) -> Option<CFRetained<CFArray<CFDictionary<CFString, CFType>>>> {
        unsafe {
            // matrix is *const CGAffineTransform, i don't care about it for now so use null
            let matrix: *const CGAffineTransform = std::ptr::null();
            let font = CTFont::with_font_descriptor(&descriptor, 0.0, matrix);

            Self::font_to_variable_axes(&font)
        }
    }

    fn get_face_index(current: &CFRetained<CTFontDescriptor>, font_url: &CFRetained<CFURL>) -> i32 {
        unsafe {
            let font_descriptors = CTFontManagerCreateFontDescriptorsFromURL(font_url);
            if let Some(descriptors) = font_descriptors {
                if descriptors.is_empty() {
                    return 0; // No descriptors found
                }

                if descriptors.count() == 1 {
                    // If there's only one descriptor, return index 0
                    return 0;
                }

                let remapped = CFRetained::cast_unchecked::<CFArray<CTFontDescriptor>>(descriptors);

                // iterate through the descriptors to find the index
                for (index, descriptor) in remapped.iter().enumerate() {
                    if &descriptor == current {
                        return index as i32; // Return the index of the matching descriptor
                    }
                }
            }
        }
        // default for now
        0
    }

    fn font_from_descriptor(
        descriptor: CFRetained<CTFontDescriptor>,
        font: Option<CFRetained<CTFont>>,
    ) -> Option<FaceInfo> {
        // Extract necessary information from the descriptor
        unsafe {
            let cf_family_name = descriptor.attribute(kCTFontFamilyNameAttribute);

            let family_name = if let Some(cf_n_type) = cf_family_name
                && let Some(cf_name) = cf_n_type.downcast_ref::<CFString>()
            {
                cf_name.to_string()
            } else {
                return None;
            };

            // map String -> Arc<[Arc<str>]>
            let family_names: Vec<Arc<str>> = vec![family_name.into()];

            let font_url = descriptor.attribute(kCTFontURLAttribute);
            let file_url = if let Some(cf_url_type) = font_url
                && let Some(cf_url) = cf_url_type.downcast_ref::<CFURL>()
            {
                cf_url.retain()
            } else {
                // Missing or fails to downcast or whatever
                return None;
            };

            let file_index = Self::get_face_index(&descriptor, &file_url);

            let path = match file_url.to_file_path() {
                Some(path) => path,
                None => return None, // Invalid file URL
            };

            let font_traits = descriptor.attribute(kCTFontTraitsAttribute);
            let (font_weight_std, font_italic_std) = if let Some(font_traits_type) = font_traits
                && let Ok(font_traits_dict_opaque) = font_traits_type.downcast::<CFDictionary>()
            {
                let font_traits_dict = CFRetained::cast_unchecked::<CFDictionary<CFString, CFType>>(
                    font_traits_dict_opaque,
                );

                let weight: Option<CFRetained<CFType>> = font_traits_dict.get(kCTFontWeightTrait);
                let ft_weight = if let Some(weight) = weight
                    && let Some(weight_number) = weight.downcast_ref::<CFNumber>()
                {
                    let weight_value = weight_number.as_f64().unwrap();

                    normalized_weight_to_css_weight(weight_value)
                } else {
                    400 // Default weight if not found
                };

                let italic: Option<CFRetained<CFType>> = font_traits_dict.get(kCTFontSlantTrait);
                let ft_italic = if let Some(italic) = italic
                    && let Some(italic_number) = italic.downcast_ref::<CFNumber>()
                {
                    let italic_value = italic_number.as_f64().unwrap();
                    italic_value != 0.0 // non-zero should be italic, although this is very naive
                } else {
                    false // Default to not italic if not found
                };

                (ft_weight, ft_italic)
            } else {
                (400, false) // default stuff
            };

            let variable_traits = match font {
                Some(axes) => Self::font_to_variable_axes(&axes),
                None => Self::descriptor_to_variable_axes(descriptor),
            };

            // font-width for variable font, temp for now
            // although there is this trait: https://developer.apple.com/documentation/coretext/kctfontwidthtrait?language=objc
            let (font_weight, font_width) = if let Some(variable_traits) = variable_traits {
                // get wght from variable traits
                let mut weight: Option<FontAxisValues> = None;
                let mut width: Option<FontAxisValues> = None;

                for trait_dict in variable_traits {
                    if let Some(name) = trait_dict.get(kCTFontVariationAxisNameKey)
                        && let Some(name) = name.downcast_ref::<CFString>()
                    {
                        let name_str = name.to_string();
                        if name_str == "weight" && weight.is_none() {
                            let min_value = trait_dict
                                .get(kCTFontVariationAxisMinimumValueKey)
                                .and_then(|v| {
                                    let deref = v.downcast_ref::<CFNumber>();
                                    deref.and_then(|n| n.as_i32())
                                });

                            let max_value = trait_dict
                                .get(kCTFontVariationAxisMaximumValueKey)
                                .and_then(|v| {
                                    let deref = v.downcast_ref::<CFNumber>();
                                    deref.and_then(|n| n.as_i32())
                                });

                            if let (Some(min), Some(max)) = (min_value, max_value) {
                                weight = Some(FontAxisValues::Range(
                                    I16Dot16::new(min),
                                    I16Dot16::new(max),
                                ));
                            }
                        }
                        if name_str == "width" && width.is_none() {
                            let min_value = trait_dict
                                .get(kCTFontVariationAxisMinimumValueKey)
                                .and_then(|v| {
                                    let deref = v.downcast_ref::<CFNumber>();
                                    deref.and_then(|n| n.as_i32())
                                });

                            let max_value = trait_dict
                                .get(kCTFontVariationAxisMaximumValueKey)
                                .and_then(|v| {
                                    let deref = v.downcast_ref::<CFNumber>();
                                    deref.and_then(|n| n.as_i32())
                                });

                            if let (Some(min), Some(max)) = (min_value, max_value) {
                                width = Some(FontAxisValues::Range(
                                    I16Dot16::new(min),
                                    I16Dot16::new(max),
                                ));
                            }
                        }
                    }
                }

                (
                    weight.unwrap_or(FontAxisValues::Fixed(I16Dot16::new(font_weight_std))),
                    width.unwrap_or(FontAxisValues::Fixed(I16Dot16::new(100))),
                )
            } else {
                (
                    FontAxisValues::Fixed(I16Dot16::new(font_weight_std)),
                    FontAxisValues::Fixed(I16Dot16::new(100)),
                )
            };

            // get width
            let face_info = FaceInfo {
                family_names: family_names.into(),
                width: font_width,
                weight: font_weight,
                italic: font_italic_std,
                source: FontSource::File {
                    path,
                    index: file_index,
                },
            };

            Some(face_info)
        }
    }

    fn font_from_font(font: CFRetained<CTFont>) -> Option<FaceInfo> {
        // get descriptor from font
        unsafe {
            let descriptor = font.font_descriptor();

            Self::font_from_descriptor(descriptor, Some(font))
        }
    }

    fn substitute_family(family: &str) -> &'static [&'static str] {
        match family {
            // Use the one in mac
            "sans-serif" => &["SF Pro", "Helvetica Neue"],
            "serif" => &["New York", "Times new Roman", "Georgia"],
            "monospace" => &["SF Mono", "Menlo", "Courier New"],
            "cursive" => &["Apple Chancery", "Zapfino"],
            "fantasy" => &["Herculanum", "Papyrus", "Impact"],
            _ => &[],
        }
    }

    fn make_font_info(
        &self,
        name: &Box<str>,
        style: &crate::simul::FontStyle,
    ) -> CFRetained<CTFont> {
        unsafe {
            let cf_name = CFString::from_str(name);
            let cf_name_deref = cf_name.downcast_ref::<CFString>().unwrap();
            let weight = CFNumber::new_f64(css_weight_to_normalized(style.weight.round_to_inner()));
            let weight_n = weight.downcast_ref::<CFNumber>().unwrap();
            let italic = CFNumber::new_f64(if style.italic { 1.0 } else { 0.0 });
            let italic_n = italic.downcast_ref::<CFNumber>().unwrap();

            let traits_dict = CFDictionary::from_slices(
                &[kCTFontSlantTrait, kCTFontWeightTrait],
                &[italic_n, weight_n],
            );

            let traits_dict_opaque = traits_dict.as_opaque();

            let attributes = CFDictionary::from_slices(
                &[kCTFontFamilyNameAttribute, kCTFontTraitsAttribute],
                &[cf_name_deref as &CFType, traits_dict_opaque as &CFType],
            );

            let descriptor = CTFontDescriptor::with_attributes(attributes.as_opaque());

            let matrix: *const CGAffineTransform = std::ptr::null();
            CTFont::with_font_descriptor(&descriptor, 14.0, matrix)
        }
    }

    fn has_glyphs(&self, font: &CFRetained<CTFont>, codepoint: u32) -> bool {
        // convert codepoint u32 to NonNull<u16> (a.k.a UniChar)
        let (mut utf16, surrogate) = codepoint_to_utf16(codepoint);

        let mut glyphs_array: Vec<u16> = if surrogate {
            // make 2 u16 array for output
            Vec::with_capacity(2)
        } else {
            // make 1 u16 array for output
            Vec::with_capacity(1)
        };

        // convert both utf16 ([u16; 2]) and glyphs_array to NonNull<u16>
        let glyphs_ptr = glyphs_array.as_mut_ptr();
        let source_ptr = utf16.as_mut_ptr();

        let result = unsafe {
            font.glyphs_for_characters(
                NonNull::new_unchecked(source_ptr),
                NonNull::new_unchecked(glyphs_ptr),
                if surrogate { 2 } else { 1 },
            )
        };

        // let's drop the glyphs_array to avoid memory leak
        std::mem::forget(glyphs_array);
        result
    }
}

impl PlatformFontProvider for CoreTextFontProvider {
    fn fonts(&self) -> &[FaceInfo] {
        &self.fonts
    }

    fn substitute(
        &self,
        request: &mut crate::simul::FaceRequest,
    ) -> Result<(), crate::simul::SubstituteError> {
        for family in std::mem::take(&mut request.families) {
            let substitutes = Self::substitute_family(&family);
            if substitutes.is_empty() {
                request.families.push(family);
            } else {
                request
                    .families
                    .extend(substitutes.iter().copied().map(Into::into))
            }
        }

        Ok(())
    }

    fn fallback(
        &self,
        request: &crate::simul::FontFallbackRequest,
    ) -> Result<Vec<FaceInfo>, crate::simul::FallbackError> {
        let mut included_fallbacks = vec![];

        for family in &request.families {
            let font_info = self.make_font_info(family, &request.style);
            let has_glyphs = self.has_glyphs(&font_info, request.codepoint);

            if has_glyphs && let Some(font_desc) = Self::font_from_font(font_info) {
                included_fallbacks.push(font_desc);
            }
        }

        // Default to system fallback if no fonts matched
        if included_fallbacks.is_empty() {
            let helvetica: Box<str> = "Helvetica".into();
            let primary_font = self.make_font_info(&helvetica, &request.style);

            let (utf16, _) = codepoint_to_utf16(request.codepoint);
            // TODO: Use String::from_utf16
            let string_data = String::from_utf16_lossy(&utf16);
            let cf_string = CFString::from_str(&string_data);
            let str_range = CFRange::new(0, cf_string.length());
            let recommended_font = unsafe { primary_font.for_string(&cf_string, str_range) };

            if let Some(font_desc) = Self::font_from_font(recommended_font) {
                included_fallbacks.push(font_desc);
            }
        }

        Ok(deduplicate_fonts_results(included_fallbacks))
    }
}

// unsafe impl Send for CoreTextFontProvider {}
// unsafe impl Sync for CoreTextFontProvider {}
