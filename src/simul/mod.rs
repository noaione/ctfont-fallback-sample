use std::{fmt::Debug, path::PathBuf, sync::Arc};

use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FontFallbackRequest {
    pub families: Vec<Box<str>>,
    pub style: FontStyle,
    pub codepoint: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FaceRequest {
    pub families: Vec<Box<str>>,
    // TODO: script?
    pub language: Option<Box<str>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FontStyle {
    pub weight: sbr_util::math::I16Dot16,
    pub italic: bool,
}

#[derive(Debug, Clone)]
pub struct FaceInfo {
    pub family_names: Arc<[Arc<str>]>,
    // TODO: Implement font width support
    //       SRV3 (I think) and WebVTT-without-CSS don't need this but may be
    //       necessary in the future
    pub width: FontAxisValues,
    pub weight: FontAxisValues,
    pub italic: bool,
    pub source: FontSource,
}

#[derive(Debug, Clone)]
pub enum FontAxisValues {
    Fixed(sbr_util::math::I16Dot16),
    Range(sbr_util::math::I16Dot16, sbr_util::math::I16Dot16),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FontSource {
    File { path: PathBuf, index: i32 },
}

#[non_exhaustive]
#[derive(Debug, Error)]
pub enum FallbackError {
    #[error(transparent)]
    #[cfg(target_os = "macos")]
    CoreText(#[from] crate::font_provider::FallbackError),
}

#[non_exhaustive]
#[derive(Debug)]
pub enum SubstituteError {
    Error,
}

pub trait PlatformFontProvider: Debug + Send + Sync {
    // fn update_if_changed(&mut self, sbr: &Subrandr) -> Result<bool, UpdateError> {
    //     _ = sbr;
    //     Ok(false)
    // }

    fn substitute(&self, request: &mut FaceRequest) -> Result<(), SubstituteError>;
    fn fonts(&self) -> &[FaceInfo];
    fn fallback(&self, request: &FontFallbackRequest) -> Result<Vec<FaceInfo>, FallbackError>;
}
