#include<iostream>
#include <vector> 
#include <algorithm>
#include <string>
#include <random>

using namespace std;

struct Book 
{
    string name;
    int pages;

    Book()
    {
        this->name = "";
        this->pages = 0;
    }
    Book(int id, int pages)
    {
        this->name = "Book" + to_string(id);
        this->pages = pages;
    }
};

void generateBooks(vector<Book>& bookVec, bool nullifyPages)
{
    int remainingPages = 9600;
    for(int i = 0; i < bookVec.size(); i++)
    {
        int pages = rand() % min(800, remainingPages) + 1;
        if(nullifyPages) pages = 0;
        Book newBook(i, pages);
        bookVec[i] = newBook;
        remainingPages -= pages;
    }
}

bool hasPredicate(double percentage) {
    random_device rd;

    mt19937 gen(rd());

    uniform_real_distribution<double> dis(0.0, 1.0);

    double randomValue = dis(gen);

    return randomValue < (percentage / 100.0);
}

void printInit(const vector<Book>& bookVec) 
{
    cout << "(define (problem book-plan-" << bookVec.size() << ")\n";
    cout << "  (:domain book-plan)\n";
    cout << "  (:objects\n   ";
    for (int i = 0; i < bookVec.size(); i++)
    {
        cout << " " << bookVec[i].name;
    }
    cout << " - Book ; List all the books\n";
    cout << "    Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec - Month ; List all the months\n";
    cout << "  )\n\n";
    cout << "  (:init\n";

    int numBooks = bookVec.size();
    double probPrec = 20.0;
    double probPara = 10.0;
    double probGoal = 30.0;
    double probRead = 15.0;


    // Setting precedence and parallels (we treat bookVec as a topologically sorted array. 
    //Therefore we only construct relations with the rest of the elements to avoid cycles)
    for (int i = 0; i < numBooks - 1; i++)
    {
        for (int j = i+1; j < numBooks; j++)
        {
            if(hasPredicate(probPrec))
            {
               cout << "    (predecessor " << bookVec[i].name << " " << bookVec[j].name << ")\n";   
            }
            else if(hasPredicate(probPara))
            {
               cout << "    (parallel " << bookVec[i].name << " " << bookVec[j].name << ")\n";
            }
        }
    }

    // Setting goals
    for (int i = 0; i < numBooks; i++)
    {
        if(hasPredicate(probGoal))
        {
            cout << "    (goal " << bookVec[i].name << ")\n";   
        }
    }

    // Setting reads
    for (int i = 0; i < numBooks; i++){
        if(hasPredicate(probRead))
        {
            cout << "    (read " << bookVec[i].name << ")\n";   
        }
    }

    for (int i = 0; i < numBooks; i++){
        cout << "    (= (numPagesBook " << bookVec[i].name << ") " << bookVec[i].pages << ")\n";
    }

    const char* months[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
    // Initializing pagesReadMonth
    for (int i = 0; i < 12; ++i) 
    {
        cout << "    (= (pagesReadMonth " << months[i] << ") 0)\n";
    }

    // Initializing before relationships
    for (int i = 0; i < 11; ++i) 
    {
        for (int j = i + 1; j < 12; ++j) {
            cout << "    (before " << months[i] << " " << months[j] << ")\n";
        }
    }

    // Initializing sameTime relationships
    for (int i = 0; i < 12; ++i) 
    {
        cout << "    (sameTime " << months[i] << " " << months[i] << ")\n";
    }

    cout << "  )\n\n";
    cout << "  (:goal\n";
    cout << "    (not (exists (?b - Book) \n";
    cout << "      (and (goal ?b) (not (read ?b)))))\n";
    cout << "  )\n";
    cout << ")\n";
}


int main()
{
    int numBooks;
    bool nullifyPages;

    //cout << "Insert the number of books:" << endl;
    cin >> numBooks;
    //cout << "Insert if you want to nullify pages" << endl;
    cin >> nullifyPages; 

    vector<Book> bookVec = vector<Book>(numBooks);
    generateBooks(bookVec, nullifyPages);

    printInit(bookVec);
}