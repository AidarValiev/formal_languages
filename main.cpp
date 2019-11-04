#include <iostream>
#include <set>
#include <string>
#include <stack>
#include <fstream>
#include <algorithm>
#include <unordered_set>

// throws exception when input is incorrect
void raise() {
	throw "Incorrect input";
}

class Regexp {
private:
	struct node;
	struct NFA;
	std::string expression;
	int size;
	void is_correct();
public:
	Regexp() {}
	Regexp(const std::string& pattern);
	void get(std::istream& in);
	int size_of_max_subword(const std::string& word);
	friend std::istream& operator >>(std::istream& in, Regexp& regexp);
};

Regexp::Regexp(const std::string& pattern) {
	expression = pattern;
	size = expression.size();
}

std::istream& operator >>(std::istream& in, Regexp& regexp) {
	in >> regexp.expression;
	regexp.size = regexp.expression.size();
	return in;
}

// methods that getting regular expression from stream
void Regexp::get(std::istream& in) {
	in >> expression;
	size = expression.size();
}

// checks if regular expression is in correct form
void Regexp::is_correct() {
	int counter = 0;
	for (int i = 0; i < size; ++i) {
		if (expression[i] == '.' || expression[i] == '+') {
			if (counter < 2)
				raise();
			--counter;
		}
		else if (expression[i] == '*') {
			if (counter == 0)
				raise();
		}
		else if (expression[i] != '1' && (expression[i] < 'a' || expression[i] > 'c')) {
			raise();
		}
		else {
			++counter;
		}
	}
	if (counter != 1)
		raise();
}

struct Regexp::node {
	std::set<std::pair<char, node*>> go;
	bool is_term;
	node(bool is = false) : is_term(is) {}
};

struct Regexp::NFA {
	std::set<node*> term;
	std::unordered_set<node*> nodes;
	node* root;
	NFA() {}
	NFA(node* patt) : root(patt) {}
	NFA(char symb) {
		root = new node;
		nodes.insert(root);
		if (symb != '1') {
			node* child = new node(true);
			root->go.insert({ symb, child });
			nodes.insert(child);
			term.insert(child);
		}
		else {
			root->is_term = true;
			term.insert(root);
		}
	}
	static NFA* add_merge(NFA* n1, NFA* n2);
	static NFA* mult_merge(NFA* n1, NFA* n2);
	static NFA* deg_merge(NFA* n1);
	int size_of_max_pref(const std::string& word);
	void destr();
};

// applies disjunction to pair of NFAs
Regexp::NFA* Regexp::NFA::add_merge(NFA* n1, NFA* n2) {
	node* new_node = new node(n1->root->is_term || n2->root->is_term);
	for (auto it = n1->root->go.begin(); it != n1->root->go.end(); ++it) {
		new_node->go.insert(*it);
	}
	for (auto it = n2->root->go.begin(); it != n2->root->go.end(); ++it) {
		new_node->go.insert(*it);
	}
	NFA* n3 = new NFA(new_node);
	if (new_node->is_term)
	    n3->term.insert(new_node);
	for (auto it = n1->term.begin(); it != n1->term.end(); ++it)
		n3->term.insert(*it);
	for (auto it = n2->term.begin(); it != n2->term.end(); ++it)
		n3->term.insert(*it);
	for (auto it = n1->nodes.begin(); it != n1->nodes.end(); ++it)
		n3->nodes.insert(*it);
	for (auto it = n2->nodes.begin(); it != n2->nodes.end(); ++it)
		n3->nodes.insert(*it);
	n3->nodes.insert(new_node);
	delete n1;
	delete n2;
	return n3;
}

// applies concatenation to pair of NFAs
Regexp::NFA* Regexp::NFA::mult_merge(NFA* n1, NFA* n2) {
	NFA* n3 = new NFA(n1->root);
	for (auto it = n1->term.begin(); it != n1->term.end(); ++it) {
		for (auto it2 = n2->root->go.begin(); it2 != n2->root->go.end(); ++it2) {
			(*it)->go.insert(*it2);
		}
		if (n2->root->is_term)
			n3->term.insert(*it);
		else {
			(*it)->is_term = false;
		}
	}
	for (auto it = n2->term.begin(); it != n2->term.end(); ++it)
		n3->term.insert(*it);
	for (auto it = n1->nodes.begin(); it != n1->nodes.end(); ++it)
		n3->nodes.insert(*it);
	for (auto it = n2->nodes.begin(); it != n2->nodes.end(); ++it)
		n3->nodes.insert(*it);
	delete n1;
	delete n2;
	return n3;
}

//  applies closure to NFA
Regexp::NFA* Regexp::NFA::deg_merge(NFA* nfa) {
	for (auto it = nfa->term.begin(); it != nfa->term.end(); ++it) {
		if (*it != nfa->root) {
			for (auto it2 = nfa->root->go.begin(); it2 != nfa->root->go.end(); ++it2) {
				(*it)->go.insert(*it2);
			}
		}
	}
	nfa->term.insert(nfa->root);
	nfa->root->is_term = true;
	return nfa;
}

// finds size of maximal prefix of word that belongs to regular language
int Regexp::NFA::size_of_max_pref(const std::string& word) {
	std::set<node*> current_set;
	current_set.insert(root);
	bool contains_term = root->is_term;
	int ans = -1;
	if (contains_term)
		ans = 0;
	for (int i = 0; i < word.size(); ++i) {
		std::set<node*> new_set;
		contains_term = false;
		for (auto it = current_set.begin(); it != current_set.end(); ++it) {
			for (auto it2 = (*it)->go.begin(); it2 != (*it)->go.end(); ++it2) {
				if ((*it2).first == word[i]) {
					new_set.insert((*it2).second);
					contains_term = contains_term || (*it2).second->is_term;
				}
			}
		}
		if (contains_term)
			ans = i + 1;
		current_set = new_set;
	}
	return ans;
}

// deletes all nodes from NFA
void Regexp::NFA::destr() {
	for (auto it = nodes.begin(); it != nodes.end(); ++it) {
		delete (*it);
	}
}

//  finds size of maximal subword that belongs to regular language
int Regexp::size_of_max_subword(const std::string& word) {
	is_correct();
	for (int i = 0; i < word.size(); ++i) {
		if (word[i] < 'a' || word[i] > 'c')
			raise();
	}
	std::stack<NFA*> stack;
	for (int i = 0; i < size; ++i) {
		if (expression[i] == '.') {
			NFA* s_cont = stack.top();
			stack.pop();
			NFA* f_cont = stack.top();
			stack.pop();
			stack.push(NFA::mult_merge(f_cont, s_cont));
		}
		else if (expression[i] == '+') {
			NFA* s_cont = stack.top();
			stack.pop();
			NFA* f_cont = stack.top();
			stack.pop();
			stack.push(NFA::add_merge(f_cont, s_cont));
		}
		else if (expression[i] == '*') {
			NFA* cont = stack.top();
			stack.pop();
			stack.push(NFA::deg_merge(cont));
		}
		else {
			NFA* new_cont = new NFA(expression[i]);
			stack.push(new_cont);
		}
	}
	NFA* cont = stack.top();
	stack.pop();
	int ans = -1;
	for (int i = 0; i < word.size(); ++i) {
		ans = std::max(ans, cont->size_of_max_pref(word.substr(i, word.size() - i)));
	}
	cont->destr();
	return ans;
}

int main(int argc, char* argv[]) {
    if (argc > 1 && argv[1][0] == '-' && argv[1][1] == 't') {   //  checks if objective are tests
        std::ifstream fin("tests.txt");
        std::ofstream fout("output.txt");
        Regexp exp;
        std::string s;
        int counter = 0, correct_counter = 0;
        while (fin >> s) {
            int out, corr_out;
            fin >> exp >> s;
            try {
                out = exp.size_of_max_subword(s);
            }
            catch(const char* exception) {
                out = -2;
            }
            fin >> corr_out;
            if (out == corr_out) {
                ++correct_counter;
                fout << "Test " << counter + 1 << ": correct\n  Expected: ";
                if (corr_out == -2)
                    fout << "Error";
                else
                    fout << corr_out;
                fout << "\n  Received: ";
                if (out == -2)
                    fout << "Error";
                else
                    fout << out;
            }
            else {
                fout << "Test " << counter + 1 << ": correct\n  Expected: ";
                if (corr_out == -2)
                    fout << "Error";
                else
                    fout << corr_out;
                fout << "\n  Received: ";
                if (out == -2)
                    fout << "Error";
                else
                    fout << out;
            }
            fout << "\n\n";
            ++counter;
        }
        std::cout << correct_counter << '/' << counter << " tests passed\n";
    }
    else {
        Regexp exp;
        std::string s;
        std::cin >> exp >> s;
        std::cout << exp.size_of_max_subword(s) << "\n";
    }
    return 0;
}
