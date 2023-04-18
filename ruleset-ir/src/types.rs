#[derive(Debug)]
pub enum RegId {
    Reg0, // Register 0
    Reg1,
    Reg2,
    Reg3,
    Reg4,
}

#[derive(Debug)]
pub struct Label {
    _val: String,
}

#[derive(Debug)]
pub enum ReturnCode {
    Nothing,
    CondFailed,
    CondPassed,
    RsTerminated,
    Error,
}

#[derive(Debug)]
pub struct MemoryKey {
    _key: String,
}

#[derive(Debug)]
pub struct QNodeAddr {
    _val: i64,
}

#[derive(Debug, Hash)]
pub struct QubitId {
    _val: i64,
}

#[derive(Debug)]
pub enum Basis {
    Z,
    Y,
    X,
    Random,
}

// Used in ver.1 Ruleset
// This must be compatible with omnetpp simtime.
#[derive(Debug)]
pub struct Time;

#[derive(Debug)]
pub struct PurType;
