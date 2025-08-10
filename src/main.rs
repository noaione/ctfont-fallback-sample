use crate::{font_provider::CoreTextFontProvider, simul::PlatformFontProvider};

mod font_provider;
mod simul;

fn main() {
    let core_text = CoreTextFontProvider::new().unwrap();

    println!("Found {} fonts", core_text.fonts().len());
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

    let result_var_fonts = core_text
        .fallback(&crate::simul::FontFallbackRequest {
            families: vec![Box::from("Monaspace Argon Var")],
            style: crate::simul::FontStyle {
                weight: sbr_util::math::I16Dot16::new(700),
                italic: false,
            },
            codepoint: 0x0041, // 'A'
        })
        .unwrap();

    // should return Monaspace Argon Var if you have it installed
    println!("{:?}", result_var_fonts);

    // println!("Available fonts: {:#?}", core_text.fonts());
}
