#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <chrono>
#include <cstdio>
#include <cmath>
#include <cstdlib>
using namespace std;

#ifndef TIME
#define TIME 5.8
#endif

double Temp = 0.5;
double TypeProb[] = {
    1.0, // 追加
    1.7, // 削除
    1.3, // 大きく移動
    5.7, // 小さく移動
    5.0, // 回転
    1.9, // スワップ
    1.0, // 片方スワップ
};

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
    int idxF(int D) const {return z*D+x;}
    int idxR(int D) const {return z*D+y;}
    int idxA(int D) const {return x*D*D+y*D+z;}
    bool inBox(int D) const {return 0<=x && x<D && 0<=y && y<D && 0<=z && z<D;}
};

static const Point DP[] = {
    Point(1, 0, 0),
    Point(-1, 0, 0),
    Point(0, 1, 0),
    Point(0, -1, 0),
    Point(0, 0, 1),
    Point(0, 0, -1),
};

struct Score
{
    long long score;
    int r[2];
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

// Pからブロックを広げる。
void expand(int D, vector<int> F[2], vector<int> R[2], vector<Point> P[2], vector<int> RT[2], vector<int> A[2])
{
    int n = (int)P[0].size();

    static vector<vector<Point>> B;
    if (B.size()<n)
        B.resize(n);
    for (int i=0; i<n; i++)
        B[i].clear();
    static vector<int> checked;
    checked.clear();
    checked.resize(n);
    static vector<int> checked_d;
    checked_d.clear();
    checked_d.resize(n);

    for (int o=0; o<2; o++)
    {
        A[o].clear();
        A[o].resize(D*D*D);
    }

    for (int i=0; i<n; i++)
    {
        B[i].push_back(Point(0, 0, 0));
        for (int o=0; o<2; o++)
            A[o][P[o][i].idxA(D)] = i+1;
    }

    while (true)
    {
        int i;
        Point p;
        for (i=0; i<n; i++)
            for (; checked[i]<(int)B[i].size(); checked[i]++)
            {
                Point o = B[i][checked[i]];

                for (; checked_d[i]<6; checked_d[i]++)
                {
                    bool ok = true;
                    for (int j=0; j<2 && ok; j++)
                    {
                        Point p = P[j][i]+rotate(o+DP[checked_d[i]], RT[j][i]);
                        if (!p.inBox(D) ||
                            A[j][p.idxA(D)]!=0 ||
                            F[j][p.idxF(D)]==0 ||
                            R[j][p.idxR(D)]==0)
                            ok = false;
                    }
                    if (!ok)
                        continue;
                    p = o+DP[checked_d[i]];
                    goto ok;
                }
                checked_d[i] = 0;
            }
        break;
    ok:;
        B[i].push_back(p);

        for (int o=0; o<2; o++)
        {
            Point t = P[o][i]+rotate(p, RT[o][i]);
            A[o][t.idxA(D)] = i+1;
        }
    }

    // シルエットに足りない部分を埋める。
    for (int i=0; i<2; i++)
    {
        static vector<int> Fok;
        Fok.clear();
        Fok.resize(D*D);
        static vector<int> Rok;
        Rok.clear();
        Rok.resize(D*D);

        for (int z=0; z<D; z++)
            for (int y=0; y<D; y++)
                for (int x=0; x<D; x++)
                    if (A[i][x*D*D+y*D+z]!=0)
                    {
                        Fok[z*D+x] = 1;
                        Rok[z*D+y] = 1;
                    }

        int id = n;
        for (int z=0; z<D; z++)
            for (int x=0; x<D; x++)
                if (F[i][z*D+x]!=0 && Fok[z*D+x]==0)
                    for (int j=0; j<2; j++)
                        for (int y=0; y<D && Fok[z*D+x]==0; y++)
                            if (R[i][z*D+y]!=0 && (j!=0 || Rok[z*D+y]==0))
                            {
                                A[i][x*D*D+y*D+z] = id+++1;
                                Fok[z*D+x] = 1;
                                Rok[z*D+y] = 1;
                            }
        for (int z=0; z<D; z++)
            for (int y=0; y<D; y++)
                if (R[i][z*D+y]!=0 && Rok[z*D+y]==0)
                    for (int x=0; x<D && Rok[z*D+y]==0; x++)
                        if (F[i][z*D+x]!=0)
                        {
                            A[i][x*D*D+y*D+z] = id+++1;
                            Fok[z*D+x] = 1;
                            Rok[z*D+y] = 1;
                        }
    }
}

void calcScore(int D, vector<int> A[2], Score *s)
{
    static vector<int> N[2];
    for (int i=0; i<2; i++)
        N[i].clear();

    for (int i=0; i<2; i++)
        for (int x=0; x<D; x++)
            for (int y=0; y<D; y++)
                for (int z=0; z<D; z++)
                {
                    int a = A[i][x*D*D+y*D+z];
                    if (a!=0)
                    {
                        a--;
                        while ((int)N[0].size()<a+1)
                            for (int o=0; o<2; o++)
                                N[o].push_back(0);
                        N[i][a]++;
                    }
                }

    for (int o=0; o<2; o++)
        s->r[o] = 0;
    s->V.clear();
    for (int i=0; i<(int)N[0].size(); i++)
    {
        if (N[0][i]>0 && N[0][i]>0)
            s->V.push_back(N[0][i]);
        else if (N[0][i]>0)
            s->r[0] += N[0][i];
        else if (N[1][i]>0)
            s->r[0] += N[1][i];
    }

    double sum = s->r[0]+s->r[1];
    for (int v: s->V)
        sum += 1./v;
    s->score = (long long)(1e9*sum);
}

void output(int D, vector<int> A[2])
{
    int n = 0;
    for (int o=0; o<2; o++)
        for (int a: A[o])
            n = max(n, a);
    cout<<n<<endl;

    for (int o=0; o<2; o++)
    {
        for (int i=0; i<D*D*D; i++)
            cout<<(i==0?"":" ")<<A[o][i];
        cout<<endl;
    }
}

int main(int argc, char **argv)
{
    if (argc>1)
    {
        Temp = atof(argv[1]);
        for (int i=1; i<7; i++)
            TypeProb[i] = atof(argv[i+1]);
    }

    int D;
    cin>>D;
    vector<int> F[2], R[2];
    F[0] = read2D(D);
    R[0] = read2D(D);
    F[1] = read2D(D);
    R[1] = read2D(D);

    chrono::system_clock::time_point start = chrono::system_clock::now();

    my_exp_init();

    int volume = D*D*D;
    for (int i=0; i<2; i++)
    {
        int v = 0;
        for (int z=0; z<D; z++)
        {
            int w = 0;
            for (int x=0; x<D; x++)
                if (F[0][z*D+x]!=0)
                    w++;
            int h = 0;
            for (int y=0; y<D; y++)
                if (R[0][z*D+y]!=0)
                    h++;
            v += w*h;
        }
        volume = min(volume, v);
    }

    vector<Point> P[2];
    vector<int> RT[2];

    auto checkPos = [&](int o, Point p) -> bool
    {
        if (F[o][p.z*D+p.x]==0 ||
            R[o][p.z*D+p.y]==0)
            return false;

        for (auto pt: P[o])
            if (pt==p)
                return false;

        return true;
    };

    auto randomPos = [&](int o) -> Point
    {
        while (true)
        {
            Point p(xor64()%D, xor64()%D, xor64()%D);
            if (checkPos(o, p))
                return p;
        }
    };

    for (int o=0; o<2; o++)
    {
        P[o].push_back(randomPos(o));
        RT[o].push_back(xor64()%24);
    }

    vector<int> bestA[2];
    Score bestScore;
    bestScore.score = 999999999999999999;

    auto evaluate = [&](vector<Point> P[2], vector<int> RT[2]) -> double
    {
        static Score score;
        static vector<int> A[2];
        expand(D, F, R, P, RT, A);

        calcScore(D, A, &score);
        if (score.score<bestScore.score)
        {
            for (int o=0; o<2; o++)
                bestA[o] = A[o];
            bestScore = score;
        }
        return score.score*1e-9;
    };
    double score = evaluate(P, RT);

    const static int typeNum = 7;
    // 遷移種別の試行回数と、スコアが向上したかどうか。
    int numTrial[typeNum];
    int numSuccess[typeNum];
    for (int i=0; i<typeNum; i++)
    {
        numTrial[i] = 1;
        numSuccess[i] = 1;
    }

    double temp_inv;
    int iter;
    for (iter=0; ; iter++)
    {
        if (iter%0x400==0)
        {
            chrono::system_clock::time_point now = chrono::system_clock::now();
            double time = chrono::duration_cast<chrono::nanoseconds>(now-start).count()*1e-9/TIME;
            if (time>1.0)
                break;
            double temp = Temp*(1.0-time);
            temp_inv = 1./temp;
        }

        static vector<Point> Pold[2];
        static vector<int> RTold[2];
        for (int o=0; o<2; o++)
        {
            Pold[o] = P[o];
            RTold[o] = RT[o];
        }

        int type;
        {
            // これまでの成功確率に応じて遷移を決める。
            //for (int i=0; i<typeNum; i++)
            //    TypeProb[i] = (double)numSuccess[i]/numTrial[i];
            //// ときどきリセット
            //if (iter%10000==0)
            //    for (int i=0; i<typeNum; i++)
            //    {
            //        numTrial[i] = 1;
            //        numSuccess[i] = 1;
            //    }

            // 固定値
            double sum = 0.;
            for (int i=0; i<typeNum; i++)
                sum += TypeProb[i];

            double p = xor64()/(double)0x80000000*sum;
            double s = 0.;
            type = 0;
            for (int i=0; i<typeNum; i++)
            {
                s += TypeProb[i];
                if (s>p)
                {
                    type = i;
                    break;
                }
            }
        }

        int target = xor64()%(int)P[0].size();
        switch (type)
        {
        case 0:
            // ブロックの追加
            if ((int)P[0].size()>=volume-2)
                continue;
            for (int o=0; o<2; o++)
            {
                P[o].push_back(randomPos(o));
                RT[o].push_back(xor64()%24);
            }
            break;
        case 1:
            // ブロックの削除
            if ((int)P[0].size()<=1)
                continue;
            for (int o=0; o<2; o++)
            {
                P[o].erase(P[o].begin()+target);
                RT[o].erase(RT[o].begin()+target);
            }
            break;
        case 2:
            // 大きく移動
            {
                int o = xor64()%2;
                P[o][target] = randomPos(o);
            }
            break;
        case 3:
            // 小さく移動
            {
                int o = xor64()%2;
                int d = xor64()%6;
                bool ok = false;
                for (int i=0; i<6; i++)
                {
                    Point p = P[o][target]+DP[(i+d)%6];
                    if (p.inBox(D) && checkPos(o, p))
                    {
                        P[o][target] = p;
                        ok = true;
                        break;
                    }
                }
                if (!ok)
                    continue;
            }
            break;
        case 4:
            // 回転
            while (true)
            {
                int o = xor64()%2;
                int r = xor64()%24;
                if (RT[o][target]!=r)
                {
                    RT[o][target] = r;
                    break;
                }
            }
            break;
        case 5:
            // スワップ
            // 順番に拡張していくので意味があるかもしれない。
            if ((int)P[0].size()<=1)
                continue;
            while (true)
            {
                int target2 = xor64()%(int)P[0].size();
                if (target2!=target)
                {
                    for (int o=0; o<2; o++)
                    {
                        swap(P[o][target], P[o][target2]);
                        swap(RT[o][target], RT[o][target2]);
                    }
                    break;
                }
            }
        case 6:
            // 対応関係をスワップ
            if ((int)P[0].size()<=1)
                continue;
            while (true)
            {
                int target2 = xor64()%(int)P[0].size();
                if (target2!=target)
                {
                    int o = xor64()%2;
                    swap(P[o][target], P[o][target2]);
                    swap(RT[o][target], RT[o][target2]);
                    break;
                }
            }
        }

        double score2 = evaluate(P, RT);

        numTrial[type]++;
        if (score2<score)
            numSuccess[type]++;

        if (score2<score ||
            //exp((score2-score)*temp_inv)*0x80000000>xor64())
            my_exp((score-score2)*temp_inv)>xor64())
        {
            score = score2;
        }
        else
        {
            for (int o=0; o<2; o++)
            {
                P[o] = Pold[o];
                RT[o] = RTold[o];
            }
        }
    }

    output(D, bestA);

    chrono::system_clock::time_point now = chrono::system_clock::now();
    double time = chrono::duration_cast<chrono::nanoseconds>(now-start).count()*1e-9;
    fprintf(
        stderr,
        "%3d %4d %.3f %8d %12lld %2d %2d %2d (",
        D,
        volume,
        time,
        iter,
        bestScore.score,
        bestScore.r[0],
        bestScore.r[1],
        (int)bestScore.V.size());
    sort(bestScore.V.begin(), bestScore.V.end());
    for (int i=0; i<(int)bestScore.V.size(); i++)
        fprintf(stderr, "%s%d", i==0?"":", ", bestScore.V[i]);
    fprintf(stderr, ")\n");

    //for (int i=0; i<typeNum; i++)
    //    fprintf(
    //        stderr,
    //        "%d %8d/%8d %8.4f\n",
    //        i,
    //        numSuccess[i],
    //        numTrial[i],
    //        100.*numSuccess[i]/numTrial[i]);
}
