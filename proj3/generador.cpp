#include<iostream>
#include <vector> 
#include <algorithm>
#include <string>

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
    int probPrec = 4;
    int probPara = 5;
    int probGoal = 3;
    int probRead = 8;

    int checkPrec = max(1, numBooks / probPrec);
    int checkPara = max(1, numBooks / probPara);
    int checkGoal = max(1, numBooks / probGoal);
    int checkRead = max(1, numBooks / probRead);


    // Setting precedence and parallels (we treat bookVec as a topologically sorted array. 
    //Therefore we only construct relations with the rest of the elements to avoid cycles)
    for (int i = 0; i < numBooks - 1; i++)
    {
        for (int j = i+1; j < numBooks; j++)
        {
            if((rand() % numBooks) < checkPrec)
            {
               cout << "    (predecessor " << bookVec[i].name << " " << bookVec[j].name << ")\n";   
            }
            else if((rand() % numBooks) < checkPara)
            {
               cout << "    (parallel " << bookVec[i].name << " " << bookVec[j].name << ")\n";
               cout << "    (parallel " << bookVec[j].name << " " << bookVec[i].name << ")\n";   
            }
        }
    }

    // Setting goals
    for (int i = 0; i < numBooks; i++)
    {
        if((rand() % numBooks) < checkGoal)
        {
            cout << "    (goal " << bookVec[i].name << ")\n";   
        }
    }

    // Setting reads
    for (int i = 0; i < numBooks; i++){
        if((rand() % numBooks) < checkRead)
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