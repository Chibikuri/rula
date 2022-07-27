#[allow(dead_code)]
#[derive(Clone)]
pub enum Tok {
    // Literature Kind
    Name { name: String },
    Qubit,                        // 'qubit'
    Int32 { value: i32 },         // i32
    Int64 { value: i64 },         // i64
    Float32 { value: f32 },       // f32
    Float64 { value: f64 },       // f64
    Complex { re: f64, im: f64 }, // complex
    PId { id: u64 },              // Process Id for the rule
    Let,                          // `let`

    // Rule Keywards
    Rule,      // `rule`
    Cond,      // `cond`
    Act,       // `act`
    Condition, // reserved(`condition`)
    Action,    // reserved(`action`)

    // Alphabetical Keywards
    True,  // `true`
    False, // `false`
    None,  // `none`
    Nil,   // reserved (`nil`)

    // Function Defininition
    Fn,  // `fn`
    Def, // `#def`

    // Deliminator Kind
    LeftBrace,    // `{`
    LeftBracket,  // `<`
    LeftParen,    // `(`
    RightBrace,   // `}`
    RightBracket, // `>`
    RightParen,   // `)`

    // Async Kind
    Async,
    Await,

    // Module Kind
    Use,
    Module,

    // Operations
    If,
    Else,
    Match,
    For,
    While,
}

pub enum QubitKind {}
