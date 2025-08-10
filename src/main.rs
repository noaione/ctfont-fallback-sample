use crate::{font_provider::CoreTextFontProvider, simul::PlatformFontProvider};

mod font_provider;
mod simul;

fn main() {
    let core_text = CoreTextFontProvider::new().unwrap();

    let total_fonts = core_text.fonts().len();
    assert!(total_fonts > 0, "No fonts found in CoreText");
    println!("Total fonts available: {}", total_fonts);
    let result = core_text
        .fallback(&crate::simul::FontFallbackRequest {
            families: vec![Box::from("Helvetica")],
            style: crate::simul::FontStyle {
                weight: sbr_util::math::I16Dot16::new(400),
                italic: false,
            },
            codepoint: 0x0041, // 'A'
        })
        .unwrap();

    // Should return Helvetica
    println!("{:?}", result);
    assert_eq!(result[0].family_names[0], "Helvetica".into());
    match &result[0].source {
        crate::simul::FontSource::File { path, index } => {
            assert_eq!(
                path.to_string_lossy(),
                "/System/Library/Fonts/Helvetica.ttc"
            );
            assert_eq!(*index, 0);
        }
    }

    let result_emoji = core_text
        .fallback(&crate::simul::FontFallbackRequest {
            families: vec![Box::from("Helvetica")],
            style: crate::simul::FontStyle {
                weight: sbr_util::math::I16Dot16::new(400),
                italic: false,
            },
            codepoint: 0x1F600, // 😀
        })
        .unwrap();

    // should return Apple Color Emoji
    println!("{:?}", result_emoji);
    assert_eq!(result_emoji[0].family_names[0], "Apple Color Emoji".into());
    match &result_emoji[0].source {
        crate::simul::FontSource::File { path, index } => {
            assert_eq!(
                path.to_string_lossy(),
                "/System/Library/Fonts/Apple Color Emoji.ttc"
            );
            assert_eq!(*index, 0);
        }
    }

    let result_jp = core_text
        .fallback(&crate::simul::FontFallbackRequest {
            families: vec![Box::from("Helvetica")],
            style: crate::simul::FontStyle {
                weight: sbr_util::math::I16Dot16::new(400),
                italic: false,
            },
            codepoint: 0x3042, // あ
        })
        .unwrap();

    // should return Hiragino Sans
    println!("{:?}", result_jp);
    assert_eq!(result_jp[0].family_names[0], "Hiragino Sans".into());
    match &result_jp[0].source {
        crate::simul::FontSource::File { path, index } => {
            assert_eq!(
                path.to_string_lossy(),
                "/System/Library/Fonts/ヒラキ\u{3099}ノ角コ\u{3099}シック W3.ttc"
            );
            assert_eq!(*index, 0);
        }
    }

    // let result_var_fonts = core_text
    //     .fallback(&crate::simul::FontFallbackRequest {
    //         families: vec![Box::from("Monaspace Argon Var")],
    //         style: crate::simul::FontStyle {
    //             weight: sbr_util::math::I16Dot16::new(700),
    //             italic: false,
    //         },
    //         codepoint: 0x0041, // 'A'
    //     })
    //     .unwrap();

    // // should return Monaspace Argon Var if you have it installed
    // println!("{:?}", result_var_fonts);

    let results_ttc_fonts = core_text
        .fallback(&crate::simul::FontFallbackRequest {
            families: vec![Box::from("Helvetica")], // should have TTC fonts
            style: crate::simul::FontStyle {
                weight: sbr_util::math::I16Dot16::new(400),
                italic: true,
            },
            codepoint: 0x0041, // 'A'
        })
        .unwrap();

    // Helvetica with index 0
    println!("{:?}", results_ttc_fonts);
    assert_eq!(results_ttc_fonts[0].family_names[0], "Helvetica".into());
    match &results_ttc_fonts[0].source {
        crate::simul::FontSource::File { path, index } => {
            assert_eq!(
                path.to_string_lossy(),
                "/System/Library/Fonts/Helvetica.ttc"
            );
            assert_eq!(*index, 2); // Italic variant in the TTC
        }
    }

    // println!("Available fonts: {:#?}", core_text.fonts());
}
