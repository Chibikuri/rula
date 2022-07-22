struct Example {
    val: i32
}

impl Example {
    fn new(num: i32) -> Example {
        Example { val:num }
    }

    fn return_val(&self) -> i32{
        return self.val
    }
}
 
fn main() {
    let ex = Example::new(32);
    println!("target; {}", ex.return_val());
    println!("Hello, world!");
}

#[cfg(test)]
mod test{
    use super::*;

    #[test]
    fn test_example(){
        let expected = 10;
        let actual = Example::new(10);
        assert_eq!(expected, actual.return_val());
    }
}


