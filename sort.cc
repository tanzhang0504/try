#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>

using namespace std;

void PrintVec(const vector<int> &vec) {
	for (auto val : vec) {
		cout << val << " ";
	}
	cout << endl;
}

void PrintArr(int *arr, int n) {
	for (int i = 0; i < n; i++) {
		cout << arr[i] <<  " ";
	}
	cout << endl;
}

void BubbleSort(int *arr, int n) {
	int i = n - 1;
	while (i > 0) {
		int swap_ind = 0;
		for (int j = 1; j <= i; j++) {
			if (arr[j-1] > arr[j]) {
				int tmp = arr[j-1];
				arr[j-1] = arr[j];
				arr[j] = tmp;
				swap_ind = j;
			}
		} 
		i = swap_ind;
	}
}

void SelectionSort(int *arr, int n) {
	for (int i = 0; i < n - 1; i++) {
		int min_ind = i;
		for (int j = i + 1; j < n; j++) {
			if (arr[min_ind] > arr[j]) {
				min_ind = j;
			}
		}
		swap(arr[i], arr[min_ind]);
	}
}

void InsertionSort(int *arr, int n) {
	for (int i = 1; i < n; i++) {
		int val = arr[i];
		int j = -1;
		for (j = i - 1; j >= 0; j--) {
			if (arr[j] > val) {
				arr[j+1] = arr[j];
			} else {
				break;
			}
		}
		arr[++j] = val;
	}
}

void Merge(int *in, int start, int middle, int end, int *out) {
	int i = start, j = middle+1, k = start; 
	while (i <= middle || j <= end) {
		if (i <= middle && (in[i] <= in[j] || j > end)) {
			out[k++] = in[i++];
		} else {
			out[k++] = in[j++];
		}
	}
}

void MergeSort(int *in, int start, int end, int *out) {
	if (start >= end) {
		return;
	}
	int middle = (start + end)/2;
	MergeSort(in, start, middle, out);
	MergeSort(in, middle+1, end, out);
	Merge(in, start, middle, end, out);
	memcpy(&in[start], &out[start], sizeof(int) * (end - start + 1));
}

void MergeSort(int *arr, int n) {
	if (n <= 0) return;
	int *tmp = new int[n];
	MergeSort(arr, 0, n-1, tmp);
	delete[] tmp;
}

int Partition(int *arr, int n, int pivot_ind) {
	int swap_ind = 0;
	swap(arr[pivot_ind], arr[n-1]);
	pivot_ind = n-1;
	for (int i = 0; i < n-1; i++) {
		if (arr[i] < arr[pivot_ind]) {
			swap(arr[i], arr[swap_ind++]);
		}
	}
	swap(arr[swap_ind], arr[pivot_ind]);
	return swap_ind;
}

void QuickSort(int *arr, int n) {
	if (n <= 1) return;
	int pivot_ind = 0;
	pivot_ind = Partition(arr, n, pivot_ind);
	QuickSort(arr, pivot_ind);
	QuickSort(&arr[pivot_ind + 1], n - pivot_ind - 1);
}


void BucketSort(int *arr, int start, int end, int k, int *tmp) {
}

void BucketSort(int *arr, int n, int k=4) {
	
}

int FindNumber(int *arr, int start, int end, int k) {
	if (start > end) return -1;
	int pivot_val = arr[end];
	int ind = start;
	for (int i = start; i < end; i++) {
		if (arr[i] < pivot_val) {
			swap(arr[i], arr[ind++]);
		}
	}
	swap(arr[ind], arr[end]);
	if (ind == k) {
		return arr[ind];
	}
	if (k > ind) {
		return FindNumber(arr, ind+1, end, k);
	} else {
		return FindNumber(arr, start, ind-1, k);
	}
}

int FindMedian(int *arr, int n) {
	return FindNumber(arr, 0, n - 1, n/2);
}

typedef void (*func)(int*, int n);

struct Test {
	Test(const vector<int> &vec) {
		n = vec.size();
		if (n == 0) return;
		arr = new int[n];
		for (int i = 0; i < n; i++) {
			arr[i] = vec[i];
		}
	}

	~Test() {
		if (n > 0) {
			delete[] arr;
		}
	}
	int *arr;
	int n;
};

void TestSort(func sort_func) {
	Test tests[] = {
		vector<int>{2, 3, 1, 4, 8},
		vector<int>{3, 2, 1, 4, 8},
		vector<int>{1, 2, 3, 4},
		vector<int>{5, 4, 3, 2, -12},
		vector<int>{2, 4, 3, 1},
		vector<int>{15, 11, 7, 6, 4, 3, -1, -100},
		vector<int>{3, 1, 1, 8, -10, -77, -99, -14, -18},
		vector<int>{1},
		vector<int>{1, 1, 1},
		vector<int>{-9, -1, -1, -2, -5, -4, -2},
		vector<int>{},
	};
	for (auto &test : tests) {
		vector<int> want(test.arr, test.arr + test.n);
		sort(want.begin(), want.end());
		sort_func(test.arr, test.n);
		vector<int> got(test.arr, test.arr + test.n);
		if (got != want) {
			cout << "Error: got: ";
			PrintVec(got);
			cout << "Error: want: ";
			PrintVec(want);
		}
	}
	cout << sizeof(tests)/sizeof(tests[0]) << " tests done!" << endl;
}

void TestFindMedian() {
	Test tests[] = {
		vector<int>{2, 3, 1, 4, 8},
		vector<int>{3, 2, 1, 4, 8},
		vector<int>{5, 4, 3, 2, -12},
		vector<int>{15, 11, 7, 6, 4, 3, -1, -100, -77},
		vector<int>{3, 1, 1, 8, -10, -77, -99, -14, -18},
		vector<int>{1},
		vector<int>{1, 1, 1},
		vector<int>{-9, -1, -1, -2, -5, -4, -2},
		vector<int>{1, 3, 1, 1, -1},
	};
	for (auto &test : tests) {
		vector<int> want(test.arr, test.arr + test.n);
		sort(want.begin(), want.end());
		int want_m = want[want.size()/2];
		int got_m = FindMedian(test.arr, test.n);
		if (got_m != want_m) {
			cout << "Error: got: " << got_m << " want: " << want_m << endl;
		}
	}
	cout << sizeof(tests)/sizeof(tests[0]) << " tests done!" << endl;
}

int main(int argc, char **argv) {
	//TestSort(BubbleSort);
	TestSort(SelectionSort);
	TestSort(InsertionSort);
	//TestSort(MergeSort);
	TestSort(QuickSort);
	//TestFindMedian();
	return 0;
}

