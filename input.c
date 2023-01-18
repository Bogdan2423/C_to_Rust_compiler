int is_prime(int a){
    if (a%2==0)
        return 0;
    for (int i = 3; i<a; i+=2)
        if ((a%i)==0)
            return 0;
    return 1;
}

float increment(float num){
    num++;
    num++;
    return num;
}

int main(){
    float c = 24;
    int a = 10;

    c = increment(c);

    a = c/(1+1);

    if (is_prime(a)){
        printf("%d", a);
    }
    else if (c>14){
        printf("c = %d", c);
    }
    else{
        printf("asdf");
    }

    return 0;
}