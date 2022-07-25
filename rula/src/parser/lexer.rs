use super::token::Tok;
use phf::phf_map;

// static KEYWORDS: phf::Map<&'static str, Tok> = phf_map! {
//     "loop" => Tok::Loop,
//     "continue" => Tok::Continue,
//     "break" => Tok::Break,
//     "fn" => Tok::Fn,
//     "extern" => Tok::Extern,
// };

// pub fn parse_keyword(keyword: &str) -> Option<Tok> {
//     KEYWORDS.get(keyword).cloned()
// }