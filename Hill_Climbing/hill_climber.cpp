#include <iostream>
#include <vector>
#include <cmath>
using namespace std;

vector<int> MutateBits(vector<int>& vec){
    vector<int> changebit {1, 0};
    int randomNo = rand();
    vec[randomNo%100] = changebit[randomNo%2];
    return vec;
}

int easyEval(vector<int> & vec){
    int fitness = 0;
    for (int i=0; i < vec.size(); i++){
        if (i%2 == 0) {
            fitness += vec[i];
        } else {
            fitness -= vec[i];
        }
    }
    return max(0, 2*fitness);
}

int hardEval(vector<int> & vec){
    int fitness = 0;
    for (int i=0; i < vec.size()/2; i++){
        fitness += vec[i];
    }
    return max(0, 2*fitness);
}

int main(){
    
    return 0;
}