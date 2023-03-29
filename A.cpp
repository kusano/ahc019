#include <iostream>
#include <vector>
#include <string>
#include <utility>
#include <algorithm>
using namespace std;

struct Point
{
    int x, y, z;

    Point(): x(0), y(0), z(0) {}
    Point(int x, int y, int z): x(x), y(y), z(z) {}

    bool operator==(const Point &p) const {return x==p.x && y==p.y && z==p.z;}
    Point operator+(const Point &p) const {return Point(x+p.x, y+p.y, z+p.z);}
};

struct Score
{
    long long score;
    int r1;
    int r2;
    vector<int> V;
};

int xor64() {
    static uint64_t x = 88172645463345263ULL;
    x ^= x<<13;
    x ^= x>> 7;
    x ^= x<<17;
    return int(x&0x7fffffff);
}

vector<int> read2D(int D)
{
    vector<int> F(D*D);
    for (int z=0; z<D; z++)
    {
        string s;
        cin>>s;
        for (int x=0; x<D; x++)
            F[z*D+x] = s[x]-'0';
    }
    return F;
}

// P1, P2からブロックを広げる。
pair<vector<int>, vector<int>> expand(
    int D,
    const vector<int> &F1, const vector<int> &R1,
    const vector<int> &F2, const vector<int> &R2,
    const vector<Point> &P1,
    const vector<Point> &P2)
{
    static const Point DD[] = {
        Point(1, 0, 0),
        Point(-1, 0, 0),
        Point(0, 1, 0),
        Point(0, -1, 0),
        Point(0, 0, 1),
        Point(0, 0, -1),
    };

    int n = (int)P1.size();
    vector<vector<Point>> B(n);
    vector<int> A1(D*D*D), A2(D*D*D);

    for (int i=0; i<n; i++)
    {
        B[i].push_back(Point(0, 0, 0));
        A1[P1[i].x*D*D+P1[i].y*D+P1[i].z] = i+1;
        A2[P2[i].x*D*D+P2[i].y*D+P2[i].z] = i+1;
    }

    while (true)
    {
        int i;
        Point p;
        for (i=0; i<n; i++)
            for (Point o: B[i])
                for (Point d: DD)
                {
                    Point p1 = P1[i]+o+d;
                    if (p1.x<0 || D<=p1.x ||
                        p1.y<0 || D<=p1.y ||
                        p1.z<0 || D<=p1.z ||
                        A1[p1.x*D*D+p1.y*D+p1.z]!=0 ||
                        F1[p1.z*D+p1.x]==0 ||
                        R1[p1.z*D+p1.y]==0)
                        continue;

                    Point p2 = P2[i]+o+d;
                    if (p2.x<0 || D<=p2.x ||
                        p2.y<0 || D<=p2.y ||
                        p2.z<0 || D<=p2.z ||
                        A2[p2.x*D*D+p2.y*D+p2.z]!=0 ||
                        F2[p2.z*D+p2.x]==0 ||
                        R2[p2.z*D+p2.y]==0)
                        continue;

                    p = o+d;
                    goto ok;
                }
        break;
    ok:;
        B[i].push_back(p);

        Point t = P1[i]+p;
        A1[t.x*D*D+t.y*D+t.z] = i+1;
        t = P2[i]+p;
        A2[t.x*D*D+t.y*D+t.z] = i+1;
    }

    // シルエットに足りない部分を埋める。
    for (int i=0; i<2; i++)
    {
        const vector<int> &F = i==0?F1:F2;
        const vector<int> &R = i==0?R1:R2;
        vector<int> &A = i==0?A1:A2;

        vector<int> Fok(D*D), Rok(D*D);
        for (int z=0; z<D; z++)
            for (int y=0; y<D; y++)
                for (int x=0; x<D; x++)
                    if (A[x*D*D+y*D+z]!=0)
                    {
                        Fok[z*D+x] = 1;
                        Rok[z*D+y] = 1;
                    }

        int id = n;
        for (int z=0; z<D; z++)
            for (int x=0; x<D; x++)
                if (F[z*D+x]!=0 && Fok[z*D+x]==0)
                    for (int j=0; j<2; j++)
                        for (int y=0; y<D && Fok[z*D+x]==0; y++)
                            if (R[z*D+y]!=0 && (j!=0 || Rok[z*D+y]==0))
                            {
                                A[x*D*D+y*D+z] = id+++1;
                                Fok[z*D+x] = 1;
                                Rok[z*D+y] = 1;
                            }
        for (int z=0; z<D; z++)
            for (int y=0; y<D; y++)
                if (R[z*D+y]!=0 && Rok[z*D+y]==0)
                    for (int x=0; x<D && Rok[z*D+y]==0; x++)
                        if (F[z*D+x]!=0)
                        {
                            A[x*D*D+y*D+z] = id+++1;
                            Fok[z*D+x] = 1;
                            Rok[z*D+y] = 1;
                        }
    }

    return {A1, A2};
}

Score calcScore(int D, const vector<int> &A1, const vector<int> &A2)
{
    vector<int> N1, N2;
    for (int i=0; i<2; i++)
    {
        const vector<int> &A = i==0?A1:A2;
        vector<int> &N = i==0?N1:N2;

        for (int x=0; x<D; x++)
            for (int y=0; y<D; y++)
                for (int z=0; z<D; z++)
                {
                    int a = A[x*D*D+y*D+z];
                    if (a!=0)
                    {
                        a--;
                        while ((int)N1.size()<a+1)
                        {
                            N1.push_back(0);
                            N2.push_back(0);
                        }
                        N[a]++;
                    }
                }
    }

    Score s;
    s.r1 = 0;
    s.r2 = 0;
    for (int i=0; i<(int)N1.size(); i++)
    {
        if (N1[i]>0 && N2[i]>0)
            s.V.push_back(N1[i]);
        else if (N1[i]>0)
            s.r1 += N1[i];
        else if (N2[i]>0)
            s.r2 += N2[i];
    }

    double sum = s.r1+s.r2;
    for (int v: s.V)
        sum += 1./v;
    s.score = (long long)(1e9*sum);

    return s;
}

int main()
{
    int D;
    cin>>D;
    vector<int> F1 = read2D(D);
    vector<int> R1 = read2D(D);
    vector<int> F2 = read2D(D);
    vector<int> R2 = read2D(D);

    vector<int> bestA1;
    vector<int> bestA2;
    Score bestScore;
    bestScore.score = 999999999999999999;

    for (int iter=0; iter<256; iter++)
    {
        vector<Point> P1, P2;
        for (int i=0; i<5; i++)
        {
            Point p1, p2;
            while (true)
            {
                p1.x = xor64()%D;
                p1.y = xor64()%D;
                p1.z = xor64()%D;

                if (F1[p1.z*D+p1.x]==0 ||
                    R1[p1.z*D+p1.y]==0)
                    continue;

                bool ok = true;
                for (Point p: P1)
                    if (p1==p)
                        ok = false;
                if (!ok)
                    continue;

                p2.x = xor64()%D;
                p2.y = xor64()%D;
                p2.z = xor64()%D;

                if (F2[p2.z*D+p2.x]==0 ||
                    R2[p2.z*D+p2.y]==0)
                    continue;

                for (Point p: P2)
                    if (p2==p)
                        ok = false;
                if (!ok)
                    continue;

                break;
            }
            P1.push_back(p1);
            P2.push_back(p2);
        }

        pair<vector<int>, vector<int>> A = expand(D, F1, R1, F2, R2, P1, P2);
        vector<int> A1 = A.first;
        vector<int> A2 = A.second;

        Score score = calcScore(D, A1, A2);
        if (score.score<bestScore.score)
        {
            bestA1 = A1;
            bestA2 = A2;
            bestScore = score;
        }
    }

    int n = 0;
    for (int a: bestA1)
        n = max(n, a);
    for (int a: bestA2)
        n = max(n, a);
    cout<<n<<endl;

    for (int i=0; i<D*D*D; i++)
        cout<<(i==0?"":" ")<<bestA1[i];
    cout<<endl;
    for (int i=0; i<D*D*D; i++)
        cout<<(i==0?"":" ")<<bestA2[i];
    cout<<endl;

    cerr<<bestScore.score<<endl;
}
