use std::{
    ptr::NonNull,
    sync::{Arc, LazyLock, Once, OnceLock},
};

use objc2_core_foundation::{
    CFArray, CFDictionary, CFNumber, CFRange, CFRetained, CFString, CFType, CGAffineTransform,
    CFURL,
};
use objc2_core_text::{
    kCTFontFamilyNameAttribute, kCTFontNameAttribute, kCTFontSlantTrait, kCTFontTraitsAttribute,
    kCTFontURLAttribute, kCTFontVariationAxisMaximumValueKey, kCTFontVariationAxisMinimumValueKey,
    kCTFontVariationAxisNameKey, kCTFontWeightTrait, CTFont, CTFontCollection, CTFontDescriptor,
    CTFontManagerCreateFontDescriptorsFromURL,
};
use sbr_util::math::I16Dot16;
use thiserror::Error;

use crate::simul::{FaceInfo, PlatformFontProvider};

const CSS_WEIGHTS: [i32; 9] = [100, 200, 300, 400, 500, 600, 700, 800, 900];

type NSFontWeight = f64;

macro_rules! define_nsweights {
    ($($c_bind:ident => $name:ident,)*) => {
        extern "C" {
            $(pub static $c_bind: NSFontWeight;)*
        }

        $(static $name: LazyLock<NSFontWeight> = LazyLock::new(|| unsafe { $c_bind });)*
    }
}

define_nsweights!(
    NSFontWeightUltraLight => ULTRA_LIGHT,
    NSFontWeightThin => THIN,
    NSFontWeightLight => LIGHT,
    NSFontWeightRegular => REGULAR,
    NSFontWeightMedium => MEDIUM,
    NSFontWeightSemibold => SEMIBOLD,
    NSFontWeightBold => BOLD,
    NSFontWeightHeavy => HEAVY,
    NSFontWeightBlack => BLACK,
);

fn codepoint_to_utf16(mut value: u32) -> ([u16; 2], usize) {
    if value < 0x10000 {
        ([value as u16, 0], 1)
    } else {
        value -= 0x10000;
        (
            [
                (((value & 0b1111_1111_1100_0000_0000) >> 10) + 0xD800) as u16,
                ((value & 0b0000_0000_0011_1111_1111) + 0xDC00) as u16,
            ],
            2,
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
                crate::simul::FontAxisValues::Fixed(value) => value.to_string(),
                crate::simul::FontAxisValues::Range(min, max) => format!("{}-{}", min, max),
            };
            let weight = match &font.weight {
                crate::simul::FontAxisValues::Fixed(value) => value.to_string(),
                crate::simul::FontAxisValues::Range(min, max) => format!("{}-{}", min, max),
            };
            let italic = font.italic.to_string();
            let key = format!("{}|{}|{}|{}", family_names, width, weight, italic);

            seen.insert(key)
        })
        .collect()
}

#[derive(Debug, Error)]
pub enum NewError {
    #[error("Failed to downcast type from {0} to {1} for {2}")]
    DowncastError(&'static str, &'static str, &'static str),
    #[error("Failed to convert {0} CFNumber to f64")]
    NumberConversion(&'static str),
}

#[derive(Debug, Error)]
pub enum FallbackError {
    #[error("Failed to convert from UTF-16 to glyphs: {0}")]
    GlyphConversion(#[from] std::string::FromUtf16Error),
    #[error("Failed to convert name to CFString: {0}")]
    NameConversion(String),
    #[error("Failed to convert {0} to CFNumber: {1}")]
    I32ToCFNumber(&'static str, i32),
    #[error("Failed to downcast type from {0} to {1}")]
    DowncastError(&'static str, &'static str, &'static str),
    #[error("Failed to convert {0} to CFNumber")]
    FromCFNumber(&'static str),
}

impl From<NewError> for FallbackError {
    fn from(err: NewError) -> Self {
        match err {
            NewError::DowncastError(from_type, to_type, what) => {
                FallbackError::DowncastError(from_type, to_type, what)
            }
            NewError::NumberConversion(what) => FallbackError::FromCFNumber(what),
        }
    }
}

#[derive(Debug)]
pub struct CoreTextFontProvider {
    /// Collected fonts at startup.
    fonts: Vec<FaceInfo>,
}

/// Un-normalize the weight to CSS weight.
///
/// This utilize the real NSFontWeight constants to find which weight the
/// given weight is closest to. This is to ensure we are as accurate as possible.
fn normalized_weight_to_css_weight(weight: f64) -> i32 {
    let constants_sizes = [
        *ULTRA_LIGHT, // 100
        *THIN,        // 200
        *LIGHT,       // 300
        *REGULAR,     // 400
        *MEDIUM,      // 500
        *SEMIBOLD,    // 600
        *BOLD,        // 700
        *HEAVY,       // 800
        *BLACK,       // 900
    ];

    // find the closest weight in the constants_sizes
    let closest = constants_sizes.iter().enumerate().min_by(|(_, a), (_, b)| {
        let a_diff = (weight - *a).abs();
        let b_diff = (weight - *b).abs();
        a_diff
            .partial_cmp(&b_diff)
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    closest.map(|(index, _)| CSS_WEIGHTS[index]).unwrap_or(400) // Default to 400 if no match found
}

/// Convert CSS weight to normalized weight.
///
/// We use the NSFontWeight constants to manually map the CSS weights to normalized weights.
///
/// Verified in Swift/Objective-C in macOS 26.0
fn css_weight_to_normalized(weight: i32) -> f64 {
    // Convert CSS weight to normalized weight
    match weight {
        100 => *ULTRA_LIGHT,
        200 => *THIN,
        300 => *LIGHT,
        400 => *REGULAR,
        500 => *MEDIUM,
        600 => *SEMIBOLD,
        700 => *BOLD,
        800 => *HEAVY,
        900 => *BLACK,
        _ => *REGULAR, // Default to normal weight if out of range
    }
}

impl CoreTextFontProvider {
    pub fn new() -> Result<Self, NewError> {
        let font_collection = unsafe { CTFontCollection::from_available_fonts(None) };

        let descriptors = unsafe { font_collection.matching_font_descriptors() };
        let font_collected = if let Some(descriptors) = descriptors {
            let descriptors =
                unsafe { CFRetained::cast_unchecked::<CFArray<CTFontDescriptor>>(descriptors) };

            let mut collected_fonts = Vec::with_capacity(descriptors.len());
            // loop like this to bubble up errors
            for descriptor in descriptors {
                match Self::font_from_descriptor(descriptor, None)? {
                    Some(face_info) => collected_fonts.push(face_info),
                    None => continue,
                };
            }

            collected_fonts.shrink_to_fit();
            collected_fonts
        } else {
            Vec::new()
        };

        Ok(Self {
            fonts: deduplicate_fonts_results(font_collected),
        })
    }

    fn font_to_variable_axes(
        font: &CFRetained<CTFont>,
    ) -> Option<CFRetained<CFArray<CFDictionary<CFString, CFType>>>> {
        let variation_axes = unsafe { font.variation_axes() };
        variation_axes.map(|axes| unsafe {
            CFRetained::cast_unchecked::<CFArray<CFDictionary<CFString, CFType>>>(axes)
        })
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
            let current_name = current.attribute(kCTFontNameAttribute).and_then(|name| {
                let downcast = name.downcast_ref::<CFString>();
                downcast.map(|s| s.to_string())
            });

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
                    let descriptor_name =
                        descriptor.attribute(kCTFontNameAttribute).and_then(|name| {
                            let downcast = name.downcast_ref::<CFString>();
                            downcast.map(|s| s.to_string())
                        });
                    if descriptor_name == current_name {
                        return index as i32;
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
    ) -> Result<Option<FaceInfo>, NewError> {
        // Extract necessary information from the descriptor
        unsafe {
            let cf_family_name = descriptor.attribute(kCTFontFamilyNameAttribute);

            let family_name = if let Some(cf_n_type) = cf_family_name {
                match cf_n_type.downcast::<CFString>() {
                    Ok(cf_name) => cf_name.to_string(),
                    Err(_) => {
                        return Err(NewError::DowncastError(
                            "CFType",
                            "CFString",
                            "kCTFontFamilyNameAttribute",
                        ));
                    }
                }
            } else {
                return Ok(None);
            };

            // map String -> Arc<[Arc<str>]>
            let family_names: Vec<Arc<str>> = vec![family_name.into()];

            let font_url = descriptor.attribute(kCTFontURLAttribute);
            let file_url = if let Some(cf_url_type) = font_url {
                match cf_url_type.downcast::<CFURL>() {
                    Ok(cf_url) => cf_url,
                    Err(_) => {
                        return Err(NewError::DowncastError(
                            "CFType",
                            "CFURL",
                            "kCTFontURLAttribute",
                        ));
                    }
                }
            } else {
                // Missing or fails to downcast or whatever
                return Ok(None);
            };

            let file_index = Self::get_face_index(&descriptor, &file_url);

            let path = match file_url.to_file_path() {
                Some(path) => path,
                None => return Ok(None), // Invalid file URL
            };

            let font_traits = descriptor.attribute(kCTFontTraitsAttribute);
            let (font_weight_std, font_italic_std) = if let Some(font_traits_type) = font_traits {
                let font_traits_dict_opaque =
                    font_traits_type.downcast::<CFDictionary>().map_err(|_| {
                        NewError::DowncastError("CFType", "CFDictionary", "kCTFontTraitsAttribute")
                    })?;
                let font_traits_dict = CFRetained::cast_unchecked::<CFDictionary<CFString, CFType>>(
                    font_traits_dict_opaque,
                );

                let weight: Option<CFRetained<CFType>> = font_traits_dict.get(kCTFontWeightTrait);
                let ft_weight = if let Some(weight) = weight {
                    let weight_value = match weight.downcast::<CFNumber>() {
                        Ok(weight_number) => weight_number
                            .as_f64()
                            .ok_or(NewError::NumberConversion("kCTFontWeightTrait"))?,
                        Err(_) => {
                            return Err(NewError::DowncastError(
                                "CFType",
                                "CFNumber",
                                "kCTFontWeightTrait",
                            ));
                        }
                    };

                    normalized_weight_to_css_weight(weight_value)
                } else {
                    400 // Default weight if not found
                };

                let italic: Option<CFRetained<CFType>> = font_traits_dict.get(kCTFontSlantTrait);
                let ft_italic = if let Some(italic) = italic {
                    match italic.downcast::<CFNumber>() {
                        Ok(ital_val) => {
                            ital_val
                                .as_f64()
                                .ok_or(NewError::NumberConversion("kCTFontSlantTrait"))?
                                != 0.0 // non-zero should be italic, although this is very naive
                        }
                        Err(_) => {
                            return Err(NewError::DowncastError(
                                "CFType",
                                "CFNumber",
                                "kCTFontSlantTrait",
                            ));
                        }
                    }
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
                let mut weight: Option<crate::simul::FontAxisValues> = None;
                let mut width: Option<crate::simul::FontAxisValues> = None;

                for trait_dict in variable_traits {
                    if let Some(name) = trait_dict.get(kCTFontVariationAxisNameKey) {
                        match name.downcast::<CFString>() {
                            Ok(name) => {
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
                                        weight = Some(crate::simul::FontAxisValues::Range(
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
                                        width = Some(crate::simul::FontAxisValues::Range(
                                            I16Dot16::new(min),
                                            I16Dot16::new(max),
                                        ));
                                    }
                                }
                            }
                            Err(_) => {
                                return Err(NewError::DowncastError(
                                    "CFType",
                                    "CFString",
                                    "kCTFontVariationAxisNameKey",
                                ));
                            }
                        }
                    }
                }

                (
                    weight.unwrap_or(crate::simul::FontAxisValues::Fixed(I16Dot16::new(
                        font_weight_std,
                    ))),
                    width.unwrap_or(crate::simul::FontAxisValues::Fixed(I16Dot16::new(100))),
                )
            } else {
                (
                    crate::simul::FontAxisValues::Fixed(I16Dot16::new(font_weight_std)),
                    crate::simul::FontAxisValues::Fixed(I16Dot16::new(100)),
                )
            };

            Ok(Some(FaceInfo {
                family_names: family_names.into(),
                width: font_width,
                weight: font_weight,
                italic: font_italic_std,
                source: crate::simul::FontSource::File {
                    path,
                    index: file_index,
                },
            }))
        }
    }

    fn font_from_font(font: CFRetained<CTFont>) -> Result<Option<FaceInfo>, FallbackError> {
        // get descriptor from font
        unsafe {
            let descriptor = font.font_descriptor();

            Self::font_from_descriptor(descriptor, Some(font)).map_err(FallbackError::from)
        }
    }

    #[expect(dead_code)]
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
        name: &str,
        style: &crate::simul::FontStyle,
    ) -> Result<CFRetained<CTFont>, FallbackError> {
        unsafe {
            let cf_name = CFString::from_str(name);
            let cf_name_deref = cf_name
                .downcast_ref::<CFString>()
                .ok_or_else(|| FallbackError::NameConversion(name.to_string()))?;
            let weight = CFNumber::new_f64(css_weight_to_normalized(style.weight.round_to_inner()));
            let weight_n = weight.downcast_ref::<CFNumber>().ok_or_else(|| {
                FallbackError::I32ToCFNumber("weight", style.weight.round_to_inner())
            })?;
            let italic = CFNumber::new_f64(if style.italic { 1.0 } else { 0.0 });
            let italic_n = italic.downcast_ref::<CFNumber>().ok_or({
                FallbackError::I32ToCFNumber("italic", if style.italic { 1 } else { 0 })
            })?;

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
            Ok(CTFont::with_font_descriptor(&descriptor, 14.0, matrix))
        }
    }

    fn has_glyphs(&self, font: &CFRetained<CTFont>, codepoint: u32) -> bool {
        // convert codepoint u32 to NonNull<u16> (a.k.a UniChar)
        let (mut utf16, size) = codepoint_to_utf16(codepoint);

        let mut glyphs_array: Vec<u16> = Vec::with_capacity(size);

        // convert both utf16 ([u16; 2]) and glyphs_array to NonNull<u16>
        let glyphs_ptr = glyphs_array.as_mut_ptr();
        let source_ptr = utf16.as_mut_ptr();

        let result = unsafe {
            font.glyphs_for_characters(
                NonNull::new_unchecked(source_ptr),
                NonNull::new_unchecked(glyphs_ptr),
                size as isize,
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
            let font_info = self.make_font_info(family, &request.style)?;
            let has_glyphs = self.has_glyphs(&font_info, request.codepoint);

            if has_glyphs {
                if let Some(font_desc) = Self::font_from_font(font_info)? {
                    included_fallbacks.push(font_desc);
                }
            }
        }

        // Default to system fallback if no fonts matched
        if included_fallbacks.is_empty() {
            let helvetica: Box<str> = "Helvetica".into();
            let primary_font = self.make_font_info(&helvetica, &request.style)?;

            let (utf16, _) = codepoint_to_utf16(request.codepoint);
            let string_data = String::from_utf16(&utf16).map_err(FallbackError::GlyphConversion)?;
            let cf_string = CFString::from_str(&string_data);
            let str_range = CFRange::new(0, cf_string.length());
            let recommended_font = unsafe { primary_font.for_string(&cf_string, str_range) };

            if let Some(font_desc) = Self::font_from_font(recommended_font)? {
                included_fallbacks.push(font_desc);
            }
        }

        Ok(deduplicate_fonts_results(included_fallbacks))
    }
}

// Should be fine? Since it's only holding FaceInfo
unsafe impl Send for CoreTextFontProvider {}
unsafe impl Sync for CoreTextFontProvider {}

#[cfg(test)]
mod tests {
    use super::{css_weight_to_normalized, normalized_weight_to_css_weight};

    #[test]
    fn test_css_weight_to_normalized() {
        fn round_two(digits: f64) -> f64 {
            // round -0.800000011920929 to -0.8
            // round 0.23000000417232513 to 0.23
            (digits * 100.0).round() / 100.0
        }

        let weights = [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 50];
        let expected_results = [-0.8, -0.6, -0.4, 0.0, 0.23, 0.3, 0.4, 0.56, 0.62, 0.0, 0.0];

        for (weight, expected) in weights.iter().zip(expected_results.iter()) {
            let result = css_weight_to_normalized(*weight);
            assert_eq!(round_two(result), *expected, "Weight: {}", weight);
        }
    }

    #[test]
    fn test_normalized_weight_to_css_weight() {
        let weights = [-1.0, -0.8, -0.6, -0.4, 0.0, 0.23, 0.3, 0.4, 0.56, 0.62, 1.0];
        let expected_results = [100, 100, 200, 300, 400, 500, 600, 700, 800, 900, 900];

        for (weight, expected) in weights.iter().zip(expected_results.iter()) {
            let result = normalized_weight_to_css_weight(*weight);
            assert_eq!(result, *expected, "Weight: {} | Result: {}", weight, result);
        }
    }
}
