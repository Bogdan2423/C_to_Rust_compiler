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
    printf("%d", is_prime(c));

    if (1){
    int a = 1;
    }
    else if (2==0){
    int a = 2;
    }
    else if (is_prime(3)){
    int a = 3;}
    else
    int b = 7;

    return 0;
}