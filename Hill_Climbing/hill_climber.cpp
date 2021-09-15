#include <iostream>
#include <vector>
#include <cmath>
using namespace std;

void printVector(vector<int>& vec) {
    for (int i = 0; i < vec.size(); i++)
        cout << vec[i] << " ";
    cout << endl;
}

vector<int> MutateBits(vector<int>& vec){
    vector<int> changebit {1, 0};
    int randomNo = rand();
    vec[randomNo%100] = changebit[randomNo%2];
    return vec;
}

vector<int> bitGenerator(int n){
    vector<int> bits(n);
    for (int i=0; i<n; i++){
        bits[i] = rand() % 2;
    }
    return bits;
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
    int iterations = 0;
    int maxSoln = 100;
    long seed = time(0);
    srand(seed);
    
    clock_t startTime = clock();
    vector<int> currentBits = bitGenerator(100);
    double currentFitness = easyEval(currentBits);
    //double currentFitness = hardEval(currentBits);
    cout << "Initial solution with fitness value of " << currentFitness << ", seeded at " << seed << endl;
    printVector(currentBits);
    vector<int> modifiedBits;
    double modifiedFitness;

    while (currentFitness < 100){
        modifiedBits = MutateBits(currentBits);
        modifiedFitness = easyEval(modifiedBits);
        //modifiedFitness = hardEval(modifiedBits);
        iterations++;
        if (modifiedFitness > currentFitness){
            currentFitness = modifiedFitness;
            currentBits = modifiedBits;
            cout << "\nFinal solution with fitness value of " << currentFitness << " (after " << iterations << " iterations):\n";
            printVector(currentBits);
        }
    }
    clock_t endTime = clock();
    double runningTime = double(endTime - startTime) / double(CLOCKS_PER_SEC);
    
    cout << "\nFinal solution with fitness value of " << currentFitness << " (after " << iterations << " iterations):\n";
    printVector(currentBits);
    cout << "\nTotal running time is: " << runningTime << " seconds.\n";

    return 0;
}
