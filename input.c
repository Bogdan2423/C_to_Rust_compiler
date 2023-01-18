int is_prime(float a){
    if (a/2==0)
        return 0;
    for (int i = 3; i<a; i+=2)
        if (a/i==0)
            return 0;
    return 1;
}

int main(){
    float c = 13;
    int a = 10;

    c = 20;
    a = 30;

    return 0;
}