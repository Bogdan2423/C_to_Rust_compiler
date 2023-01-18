fn is_prime(mut a: i32)->i32{
    if a%2==0 as i32 {
    return 0;
   }
    let mut i: i32 = (3) as i32;
    while i<(a) as i32 {
   if (a%i as i32)==0 {
    return 0;
   }
   i+=(2) as i32;
   }
    return 1;
}
fn increment(mut num: f32)->f32{
    num+= 1.0;
    num+= 1.0;
    return num;
}
fn main(){
    let mut c: f32 = (24) as f32;
    let mut a: i32 = (10) as i32;
    c = (increment(c.into())) as f32;
    a = (c/(1+1) as f32) as i32;
    if is_prime(a) != 0 {
    println!("{}", a);
}   else{
   if c>(14) as f32 {
    println!("c = {}", c);
}   else{
    println!("asdf");

   }
   }
}
