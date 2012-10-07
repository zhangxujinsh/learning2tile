package stirling2;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Queue;

public class Partition {
	private HashMap<Short, short[]> neighbors;
	
	
	/**
	 * n by m
	 * @param n
	 * @param m
	 */
	public Partition(int n, int m) {
		neighbors = new HashMap<Short, short[]>(109);

		for(int i = 0 ; i < n ; i++) {
			for(int j = 0 ; j < m ; j++) {
				short id = (short) (i*m+j);
				ArrayList<Short> nes = new ArrayList<Short>(4);
				if(i-1 >= 0) {
					nes.add((short) ((i-1)*m+j));
				} 
				if((i+1) < n) {
					nes.add((short) ((i+1)*m+j));
				}
				if((j-1) >= 0) {
					nes.add((short) ((i)*m+j-1));
				}
				if((j+1) < m) {
					nes.add((short) ((i)*m+j+1));
				}
				short[] thisnes = new short[nes.size()];
				for(int k = 0 ; k < thisnes.length ; k++) {
					thisnes[k] = nes.get(k);
				}
				neighbors.put(id, thisnes);
			}
		}
		
	}
	
	
	
	public short[][] initializePartition(int n) {
		short[][] result = new short[2][n];
		return result;
	}
	
	public short[] nextPartition(short[] k, short[] m) {
		int n = k.length;
		for(int i = n-1 ; i > 0 ; i--) {
			if(k[i] <= m[i-1]) {
					k[i]++;
					m[i] = m[i] > k[i] ? m[i] : k[i];
				for(int j = i+1; j < n ; j++) {
					k[j] = k[0];
					m[j] = m[i];
				}
				return k;
			}
		}
		
		return null;
	}
	
	
	public boolean adjacent(short[] k) {
		ArrayList<ArrayList<Short>> membership = new ArrayList<ArrayList<Short>>();
		
		for(short i = 0 ; i < k.length ; i++) {
			if(k[i] > (membership.size()-1)) {
				membership.add(new ArrayList<Short>(4));
			}
			membership.get(k[i]).add(i);
		}
		
		for(int i = 0 ; i < membership.size() ; i++) {
			if(!traverse(membership.get(i)))
				return false;
		}
		
		return true;
	}
	
	public boolean traverse(ArrayList<Short> g) {
		if(g.size() ==1)	return true;
		int n = g.size();
		short[] mark = new short[n];
		Queue<Short> q = new LinkedList<Short>();
		q.add(g.get(0));
		while(!q.isEmpty()) {
			short tq = q.poll();
			short[] nes = neighbors.get(tq);
			for(int i = 0 ; i < nes.length ; i++) {
				for(int j = 0 ; j < g.size() ; j++) {
					if(nes[i] == g.get(j)) {
						if(mark[j] == 0) {
							//not visited
							mark[j] = 1;
							q.add(g.get(j));
						}
					}
				}
			}
			
		}
		
		for(int i = 0 ; i < mark.length ; i++)
			if(mark[i] != 1)	return false;
		return true;
	}
	
	
	public boolean sameSize(short[] k) {
		short[] membership = new short[k.length];
		for(int i = 0 ; i < k.length ; i++) {
			membership[k[i]]++;
		}
		
		
		for(int i = 0 ; i < membership.length ; i++) {
			if(membership[i] != 0 && membership[i] != membership[0]) {
				return false;
			}
		}
		return true;
	}
	
	
	
	
	public static void print(short[] k) {
		for(int i = 0 ; i < k.length ; i++)
			System.out.print(k[i] + " ");
		System.out.print("\n");
		
		
	}
	
	public void write(BufferedWriter bw, short[] k) throws IOException {
		bw.write("(");
		for(int i = 0 ; i < k.length-1 ; i++) {
			bw.write(""+k[i]);
			bw.write(",");
		}
		bw.write(""+k[k.length-1]);
		bw.write(")\n");
		
	}
	
	
	
	
	public static void main(String args[]) throws Exception {
		int column = 2;
		int row = 2;
		Partition p = new Partition(column,row);
		short[][] ini = p.initializePartition(column*row);
		short[] k = ini[0];
		short[] m = ini[1];
		int sum = 1;
		File out1 = new File("E:\\R\\Learning2Tile\\tilingfuns",column+"by"+row+"-par.funs");
		File out2 = new File("E:\\R\\Learning2Tile\\tilingfuns",column+"by"+row+"-til.funs");
		File out3 = new File("E:\\R\\Learning2Tile\\tilingfuns",column+"by"+row+"-equ.funs");
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(out1));
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(out2));
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(out3));
		
		while(k != null) {
			k = p.nextPartition(k,m);
			if(k!= null )
			{
				p.write(bw1, k);
				sum++;
				if(p.adjacent(k)) {
					p.write(bw2, k);
					if(p.sameSize(k)) {
						p.write(bw3, k);
					}
				}
			} 
					
		
		}
		bw1.flush();
		bw1.close();
		bw2.flush();
		bw2.close();
		bw3.flush();
		bw3.close();
		System.out.println(sum);
	}
}
