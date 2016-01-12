#include <vector>
#include <iostream>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <sstream>
#include <utility>
#include <map>
#include <queue>
#include <sstream>
#include <cmath>

using namespace std;

template<typename T>
void PrintVec(const vector<T> &vec) {
	for (auto val : vec) {
		cout << val << " ";
	}
	cout << endl;
}


 int nthUglyNumber(int n) {
        unordered_map<int, int> m;
        int prev = 0;
        int res = 1;
        --n;
        for (int i = 0; i < n; ++i) {
						cout << "Insert m: " << prev << " " << res << endl;
            m[prev] = res;
            prev = res;
            res = INT_MAX;
            for (int div : {2, 3, 4, 5}) {
                int quo = prev / div;
								if (m.count(quo)) {
                	res = min(res, m[quo] * div);
									cout << "res: " << res << " div: " << div << " quo: " << quo << endl;
								}
            }
        }
        return res;
    }

enum Color {
	kBlack = 0,
	kWhite = 1,
};

int main(int argc, char **argv) {
	Color color = kWhite;
	switch (color) {
		case kBlack:
			cout << "Black" << endl;
			break;
		case kWhite:
			cout << "White" << endl;
			break;
		default: 
			cout << "default";
	}
	return 0;
}
