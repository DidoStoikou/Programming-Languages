
//File:Input.java
import java.io.*;
import java.util.*;

public class StayHome {

    public static class coord{
        public int x;
        public int y;
        public int t;

        public coord(int a,int b,int c){
            x=a; y=b; t=c;
        }

        public int get_x(){
            return x;
        }
        public int get_y(){
            return y;
        }
        public int get_t(){
            return t;
        }
    }

  public static class tup{
        public int x;
        public int y;

    public tup(int a,int b){
        x=a; y=b;
    }

    public int get_x(){
        return x;
    }
    public int get_y(){
        return y;
    }
  }


  public static void Printer(String Board [][],int N,int M){
      for(int i = 0; i < M; i++){
          for(int j = 0; j < N; j++){
              System.out.print((Board[i][j]));
          }
          System.out.println();
      }
      System.out.println();
  }

  public static String output(String world[][], int i, int j){
    String answer = "";
    while((world[i][j].compareTo("S")!=0) &&(world[i][j].compareTo("s")!=0)){
      String temp1;
      char temp2;
      String temp;
      temp1 = world[i][j].toUpperCase();
      temp2 = temp1.charAt(0);
      temp= String.valueOf(temp2);
      if (temp.compareTo("D")==0) {
        i--;
      }
      if (temp.compareTo("U")==0) {
        i++;
      }
      if (temp.compareTo("R")==0) {
        j--;
      }
      if (temp.compareTo("L")==0) {
        j++;
      }
      answer = temp + answer;
    }
    return answer;
  }

  public static int choose(int a,int b){
    if (a>b){
      return a;
    }
    return b;
  }

      public static boolean corona_move(int y, int z, int i, int j, String world[][], Queue <tup> corona_new) {
        tup a1= new tup(y+i, z+j);
        if ((world[y+i][z+j].compareTo(".") == 0) || (world[y+i][z+j].compareTo("T") == 0)){
          world[y+i][z+j] = "W";
          corona_new.add(a1);
        }
        else if (world[y+i][z+j].compareTo("S")==0) {
          world[y+i][z+j] = "s";
          corona_new.add(a1);
        }
        else if (world[y+i][z+j].compareTo("L") ==0) {
          world[y+i][z+j] = "l";
          corona_new.add(a1);
        }
        else if (world[y+i][z+j].compareTo("R")==0) {
          world[y+i][z+j] = "r";
          corona_new.add(a1);
        }
        else if (world[y+i][z+j].compareTo("U")==0) {
          world[y+i][z+j] = "u";
          corona_new.add(a1);
        }
        else if (world[y+i][z+j].compareTo("D")==0) {
          world[y+i][z+j] = "d";
          corona_new.add(a1);
        }
        else if ((world[y+i][z+j].compareTo("A") == 0) || (world[y+i][z+j].compareTo("DA")==0) || (world[y+i][z+j].compareTo("RA")==0) || (world[y+i][z+j].compareTo("UA")==0) || (world[y+i][z+j].compareTo("LA")==0)) {
          //world[y+i][z+j] = String.toLowerCase(world[y+i][z+j]);
          String a;
          String b;
          a = world[y+i][z+j].toLowerCase();
          b = String.valueOf(a);
          world[y+i][z+j] = b;
          corona_new.add(a1);
          return true;
        }
        return false;
      }

      public static void sotiris_move(int y,int z,int i,int j,int t,String move,String world[][],int history[][],Queue <coord> sotiris_new){
        if((world[y+i][z+j].compareTo(".")== 0) || (world[y+i][z+j].compareTo("T")== 0)){
          world[y+i][z+j] = String.valueOf(move);
          history[y+i][z+j] = t;
          coord temp1 =new coord(y+i,z+j,t);
          sotiris_new.add(temp1);
        }
        else if(world[y+i][z+j].compareTo("A")==0){
          world[y+i][z+j] = String.valueOf(move) + "A";
          history[y+i][z+j] = t;
          coord temp2 =new coord(y+i,z+j,t);
          sotiris_new.add(temp2);
        }
        else if(((world[y+i][z+j].compareTo("U") == 0) || (world[y+i][z+j].compareTo("D") == 0) || (world[y+i][z+j].compareTo("R") == 0) || (world[y+i][z+j].compareTo("L") == 0) ) && (history[y+i][z+j]==t)){
          //world[y+i][z+j] = String.valueOf(choose(world[y+i][z+j], move));
          if(world[y+i][z+j].compareTo(move) > 0){
            world[y+i][z+j] = String.valueOf(world[y+i][z+j]);
          }
          else{
            world[y+i][z+j] = String.valueOf(move);
          }
        }
        else if(((world[y+i][z+j].compareTo("DA") == 0) || (world[y+i][z+j] == "RA") || (world[y+i][z+j] == "UA") || (world[y+i][z+j] == "LA")) && (history[y+i][z+j]==t)){
          String r;
          r = world[y+i][z+j];
          char taf;
          taf = r.charAt(0);
          char m;
          m = move.charAt(0);
          String sym = "A";
          char character = sym.charAt(0);
          int m1 = m + character;
          int inum2 = Integer.parseInt(r);
          int inum3 = Integer.parseInt(String.valueOf(m1));
          int p;
          p = choose(inum2, inum3);
          world[y+i][z+j] = String.valueOf(p);
        }
        return;
      }

      public static boolean update_corona(int y, int z, String world[][], int rows, int a, Queue <tup> corona_new){
        boolean A;
        A = false;
        if (y!=0) {
          A = (corona_move(y, z, -1, 0, world, corona_new) || A);
        }
        if(y!=rows-1){
          A = (corona_move(y, z, 1, 0, world, corona_new) || A);
        }
        if(z!=0){
          A = (corona_move(y, z, 0, -1, world, corona_new) || A);
        }
        if(z!=a-1){
          A = (corona_move(y, z, 0, 1, world, corona_new) || A);
        }
        return A;
      }

      public static void update_sotiris(int y,int z,int t,String world[][],int history[][],int rows,int a,Queue <coord> sotiris_new){
        if(y != 0)
          sotiris_move(y, z, -1, 0, t, "U", world, history, sotiris_new);
        if(y!=rows-1)
          sotiris_move(y, z, 1, 0, t, "D", world, history, sotiris_new);
        if(z!=0)
          sotiris_move(y, z, 0, -1, t, "L", world, history, sotiris_new);
        if(z!=a-1)
          sotiris_move(y, z, 0, 1, t, "R", world, history, sotiris_new);
        return;
        }

    public static void main(String [] args) {
      try{
          BufferedReader in = new BufferedReader(new FileReader(args[0]));
          String line;
          int N,M,t,now,Ty,Tz;
          Ty = 0;
          Tz = 0;
          String [][] world = new String[1000][1000];
          int [][] history = new int[1000][1000];
          N = 0; M = 0; t = 0; now = 0;
          Queue <coord> sotiris = new LinkedList<>();
          Queue <coord> sotiris_new = new LinkedList<>();
          Queue <tup> corona = new LinkedList<>();
          Queue <tup> corona_new = new LinkedList<>();
          Queue <tup> airports = new LinkedList<>();
          Queue <tup> airports_new = new LinkedList<>();

          while ((line = in.readLine()) != null) {
              if (M == 0){
                  N = line.length();
              }
              for (int i=0; i<N; i++){
                  char temp = line.charAt(i);
                  world[M][i] =  String.valueOf(temp);
                  history[M][i] = 0;

                  if(temp=='T'){
                    Ty = M;
                    Tz = i;
                  }
                  else if(temp=='S') {
                    coord temp1= new coord(M,i,0);
                    sotiris.add(temp1);
                  }
                  else if (temp=='W') {
                      tup temp2 = new tup(M,i);
                      corona.add(temp2);
                  }
                  else if (temp=='A') {
                    tup temp3 = new tup(M,i);
                    airports.add(temp3);
                  }
              }
              M++;
          }
          in.close ();
          int rows;
          int a;
          rows = M;
          a = N;
          t++;
          while(world[Ty][Tz].compareTo("T") == 0){
            // Printer(world,N,M);
            while(sotiris.peek() != null){
              coord temp1= new coord(0,0,0);
              temp1 = sotiris.poll();
              int a1,a2,a3;
              a1 = temp1.get_x(); a2 = temp1.get_y(); a3 = temp1.get_t();
              if ((world[a1][a2].compareTo("S") == 0) || (world[a1][a2].compareTo("U") == 0) || (world[a1][a2].compareTo("D") == 0) || (world[a1][a2].compareTo("R") == 0) || (world[a1][a2].compareTo("L") == 0) || (world[a1][a2].compareTo("DA") == 0) || (world[a1][a2].compareTo("RA") == 0) || (world[a1][a2].compareTo("UA") == 0) || (world[a1][a2].compareTo("LA") == 0)){
                update_sotiris(a1, a2, t, world, history, M, N, sotiris_new);
              }
            }
            if ((world[Ty][Tz].compareTo("S") ==0) || (world[Ty][Tz].compareTo("U") ==0) || (world[Ty][Tz].compareTo("D") ==0) || (world[Ty][Tz].compareTo("R") ==0) || (world[Ty][Tz].compareTo("L") ==0)) {
              break;
            }
            if (sotiris_new.peek() == null) {
              System.out.println("IMPOSSIBLE");
              return;
            }
            sotiris = new LinkedList<>(sotiris_new);
            sotiris_new.clear();
            if (t%2==1 && t>=now && now!=0){
              while (airports.peek() != null){
                boolean B;
                if(t==now){
                  tup temp3 = new tup(0,0);
                  temp3 = airports.poll();
                  int c1,c2;
                  c1 = temp3.get_x();
                  c2 = temp3.get_y();
                  B = corona_move(c1, c2, 0, 0, world, airports_new);
                }
                else{
                  tup temp3= new tup(0,0);
                  temp3 = airports.poll();
                  int c1,c2;
                  c1 = temp3.get_x();
                  c2 = temp3.get_y();
                  B = update_corona(c1, c2, world, rows, a, airports_new);
                }
                if (world[Ty][Tz].compareTo("W")==0){
                  System.out.println("IMPOSSIBLE");
                return;
                }
              }
              airports = new LinkedList<>(airports_new);
              airports_new.clear();
            }
            if (t%2==0){
              while (corona.peek() !=null) {
                tup temp2= new tup(0,0);
                temp2 = corona.poll();
                int b1,b2;
                b1 = temp2.get_x();
                b2 = temp2.get_y();
                boolean B;
                B = update_corona(b1, b2, world, rows, a, corona_new);
                if (B == true && now == 0){
                  now= t+5;
                }
              }
              if (world[Ty][Tz].compareTo("W")==0){
                System.out.println("IMPOSSIBLE");
                return;
              }
              corona = new LinkedList<>(corona_new);
              corona_new.clear();
            }
              t++;
              }
              System.out.println(t);
              System.out.println(output(world, Ty, Tz));
              return;
          }
      catch(IOException e) {
               e.printStackTrace ();
           }
    }
  }
