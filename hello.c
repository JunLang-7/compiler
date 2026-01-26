int main() {
    // 这是注释
    /*这也是注释*/
    int a = 1, b = 2;
    {
        int a = 2;
        b = b + a;
    }
    return b;
}