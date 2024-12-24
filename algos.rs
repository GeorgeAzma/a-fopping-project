fn sum_n(n: u32) -> u32 {
    n * (n + 1) / 2
}

fn factorial(n: u32) -> u32 {
    let mut f = 1;
    for i in 2..=n {
        f *= i;
    }
    f
}

fn gcd(mut a: u32, mut b: u32) -> u32 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn rev_num(mut n: u32) -> u32 {
    let mut r = 0;
    while n > 0 {
        r = r * 10 + n % 10;
        n /= 10;
    }
    r
}

fn is_prime(n: u32) -> bool {
    let mut divs = 0;
    for i in 1..=n {
        if n % i == 0 {
            divs += 1;
            if divs > 2 {
                return false;
            }
        }
    }
    divs == 2
}

fn is_num_palindrome(n: u32) -> bool {
    rev_num(n) == n
}

fn largest_dig(mut n: u32) -> u32 {
    let mut r = 0;
    while n > 0 {
        r = r.max(n % 10);
        n /= 10;
    }
    r
}

fn sum_dig(mut n: u32) -> u32 {
    let mut sum = 0;
    while n > 0 {
        sum += n % 10;
        n /= 10;
    }
    sum
}

fn mul_table(n: u32) {
    for i in 1..=10 {
        println!("{}", n * i);
    }
}

fn fib(n: u32) -> u32 {
    let mut a = 0;
    let mut b = 1;
    for _ in 0..n {
        b += a;
        a = b - a;
    }
    a
}
