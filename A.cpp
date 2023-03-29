#include <iostream>
#include <vector>
#include <string>
#include <utility>
#include <algorithm>
#include <chrono>
#include <cstdio>
#include <cmath>
using namespace std;

const double TIME = 5.8;

const int expN = 1024;
const double expX = 16;
int expT[expN];

void my_exp_init()
{
    for (int x=0; x<expN; x++)
    {
        double x2 = (double)-x/expN*expX;
        double e = exp(x2)*0x80000000+.5;
        if (e>=0x7fffffff)
            expT[x] = 0x7fffffff;
        else
            expT[x] = int(e);
    }
}

//  exp(t)*0x80000000;
int my_exp(double x)
{
    int x2 = int(x/-expX*expN+.5);
    if (x2<0)
        return expT[0];
    if (x2>=expN)
        return expT[expN-1];
    return expT[x2];
}

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

Point rotate(Point p, int r)
{
    switch (r)
    {
    case  0: return Point( p.x,  p.y,  p.z);
    case  1: return Point( p.x, -p.z,  p.y);
    case  2: return Point( p.x, -p.y, -p.z);
    case  3: return Point( p.x,  p.z, -p.y);

    case  4: return Point(-p.x,  p.y, -p.z);
    case  5: return Point(-p.x,  p.z,  p.y);
    case  6: return Point(-p.x, -p.y,  p.z);
    case  7: return Point(-p.x, -p.z, -p.y);

    case  8: return Point( p.y, -p.x,  p.z);
    case  9: return Point( p.y, -p.z, -p.x);
    case 10: return Point( p.y,  p.x, -p.z);
    case 11: return Point( p.y,  p.z,  p.x);

    case 12: return Point(-p.y, -p.x, -p.z);
    case 13: return Point(-p.y,  p.z, -p.x);
    case 14: return Point(-p.y,  p.x,  p.z);
    case 15: return Point(-p.y, -p.z,  p.x);

    case 16: return Point( p.z,  p.y, -p.x);
    case 17: return Point( p.z,  p.x,  p.y);
    case 18: return Point( p.z, -p.y,  p.x);
    case 19: return Point( p.z, -p.x, -p.y);

    case 20: return Point(-p.z,  p.y,  p.x);
    case 21: return Point(-p.z, -p.x,  p.y);
    case 22: return Point(-p.z, -p.y, -p.x);
    case 23: return Point(-p.z,  p.x, -p.y);
    }
    return Point();
}

// P1, P2からブロックを広げる。
pair<vector<int>, vector<int>> expand(
    int D,
    const vector<int> &F1, const vector<int> &R1,
    const vector<int> &F2, const vector<int> &R2,
    const vector<Point> &P1, const vector<int> &RT1,
    const vector<Point> &P2, const vector<int> &RT2)
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
    vector<int> checked(n);
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
            for (; checked[i]<(int)B[i].size(); checked[i]++)
            {
                Point o = B[i][checked[i]];

                for (Point d: DD)
                {
                    Point p1 = P1[i]+rotate(o+d, RT1[i]);
                    if (p1.x<0 || D<=p1.x ||
                        p1.y<0 || D<=p1.y ||
                        p1.z<0 || D<=p1.z ||
                        A1[p1.x*D*D+p1.y*D+p1.z]!=0 ||
                        F1[p1.z*D+p1.x]==0 ||
                        R1[p1.z*D+p1.y]==0)
                        continue;

                    Point p2 = P2[i]+rotate(o+d, RT2[i]);
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
            }
        break;
    ok:;
        B[i].push_back(p);

        Point t = P1[i]+rotate(p, RT1[i]);
        A1[t.x*D*D+t.y*D+t.z] = i+1;
        t = P2[i]+rotate(p, RT2[i]);
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

void output(int D, const vector<int> &A1, const vector<int> &A2)
{
    int n = 0;
    for (int a: A1)
        n = max(n, a);
    for (int a: A2)
        n = max(n, a);
    cout<<n<<endl;

    for (int i=0; i<D*D*D; i++)
        cout<<(i==0?"":" ")<<A1[i];
    cout<<endl;
    for (int i=0; i<D*D*D; i++)
        cout<<(i==0?"":" ")<<A2[i];
    cout<<endl;
}

int main()
{
    int D;
    cin>>D;
    vector<int> F1 = read2D(D);
    vector<int> R1 = read2D(D);
    vector<int> F2 = read2D(D);
    vector<int> R2 = read2D(D);

    chrono::system_clock::time_point start = chrono::system_clock::now();

    my_exp_init();

    int volume = D*D*D;
    for (int i=0; i<2; i++)
    {
        vector<int> &F = i==0?F1:F2;
        vector<int> &R = i==0?R1:R2;
        int v = 0;
        for (int z=0; z<D; z++)
        {
            int w = 0;
            for (int x=0; x<D; x++)
                if (F[z*D+x]!=0)
                    w++;
            int h = 0;
            for (int y=0; y<D; y++)
                if (R[z*D+y]!=0)
                    h++;
            v += w*h;
        }
        volume = min(volume, v);
    }

    vector<Point> P1, P2;
    vector<int> RT1, RT2;

    auto randomPos1 = [&]() -> Point
    {
        while (true)
        {
            Point p(xor64()%D, xor64()%D, xor64()%D);
            if (F1[p.z*D+p.x]==0 ||
                R1[p.z*D+p.y]==0)
                continue;

            bool ok = true;
            for (int i=0; i<(int)P1.size() && ok; i++)
                if (P1[i]==p)
                    ok = false;
            if (!ok)
                continue;
            return p;
        }
    };

    auto randomPos2 = [&]() -> Point
    {
        while (true)
        {
            Point p(xor64()%D, xor64()%D, xor64()%D);
            if (F2[p.z*D+p.x]==0 ||
                R2[p.z*D+p.y]==0)
                continue;

            bool ok = true;
            for (int i=0; i<(int)P2.size() && ok; i++)
                if (P2[i]==p)
                    ok = false;
            if (!ok)
                continue;
            return p;
        }
    };

    P1.push_back(randomPos1());
    P2.push_back(randomPos2());
    RT1.push_back(xor64()%24);
    RT2.push_back(xor64()%24);

    vector<int> bestA1;
    vector<int> bestA2;
    Score bestScore;
    bestScore.score = 999999999999999999;

    auto evaluate = [&](const vector<Point> &P1, const vector<int> &RT1, const vector<Point> &P2, const vector<int> &RT2) -> double
    {
        pair<vector<int>, vector<int>> A = expand(D, F1, R1, F2, R2, P1, RT1, P2, RT2);
        vector<int> A1 = A.first;
        vector<int> A2 = A.second;

        Score score = calcScore(D, A1, A2);
        if (score.score<bestScore.score)
        {
            bestA1 = A1;
            bestA2 = A2;
            bestScore = score;
        }
        return score.score*1e-9;
    };
    double score = evaluate(P1, RT1, P2, RT2);

    double temp_inv;
    int iter;
    for (iter=0; ; iter++)
    {
        if (iter%0x100==0)
        {
            chrono::system_clock::time_point now = chrono::system_clock::now();
            double time = chrono::duration_cast<chrono::nanoseconds>(now-start).count()*1e-9/TIME;
            if (time>1.0)
                break;
            double temp = (1.0-time);
            temp_inv = 1./temp;
        }

        vector<Point> P1n = P1;
        vector<int> RT1n = RT1;
        vector<Point> P2n = P2;
        vector<int> RT2n = RT2;

        int target = xor64()%(int)P1n.size();
        switch (xor64()%6)
        {
        case 0:
            // ブロックの追加
            if ((int)P1n.size()>=volume-1)
                continue;
            P1n.push_back(randomPos1());
            RT1n.push_back(xor64()%24);
            P2n.push_back(randomPos2());
            RT2n.push_back(xor64()%24);
            break;
        case 1:
            // ブロックの削除
            if ((int)P1n.size()<=1)
                continue;
            P1n.erase(P1n.begin()+target);
            RT1n.erase(RT1n.begin()+target);
            P2n.erase(P2n.begin()+target);
            RT2n.erase(RT2n.begin()+target);
            break;
        case 2:
            // 1個目を移動
            P1n[target] = randomPos1();
            break;
        case 3:
            // 2個目を移動
            P2n[target] = randomPos2();
            break;
        case 4:
            // 1個目を回転
            RT1n[target] = xor64()%24;
            break;
        case 5:
            // 2個目を回転
            RT2n[target] = xor64()%24;
            break;
        }

        double scoren = evaluate(P1n, RT1n, P2n, RT2n);

        if (scoren<score ||
            //exp((score2-score)*temp_inv)*0x80000000>xor64())
            my_exp((score-scoren)*temp_inv)>xor64())
        {
            score = scoren;
            P1 = P1n;
            RT1 = RT1n;
            P2 = P2n;
            RT2 = RT2n;
        }
    }

    output(D, bestA1, bestA2);

    chrono::system_clock::time_point now = chrono::system_clock::now();
    double time = chrono::duration_cast<chrono::nanoseconds>(now-start).count()*1e-9;
    fprintf(
        stderr,
        "%3d %.3f %8d %12lld %2d %2d %2d (",
        D,
        time,
        iter,
        bestScore.score,
        bestScore.r1,
        bestScore.r2,
        (int)bestScore.V.size());
    sort(bestScore.V.begin(), bestScore.V.end());
    for (int i=0; i<(int)bestScore.V.size(); i++)
        fprintf(stderr, "%s%d", i==0?"":", ", bestScore.V[i]);
    fprintf(stderr, ")\n");
}
