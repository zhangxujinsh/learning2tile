package stirling2;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Queue;

/**
 * 
 * @author lujiang
 *
 */
public class TilingFunGen {
	private HashMap<Short, short[]> neighbors;
	
	
	public TilingFunGen() {
		super();
	}
	
	
	public void writeRectangleNeighbor(int column, int row, File outdir) throws IOException {
		neighbors = new HashMap<Short, short[]>(109);
		int n = column;
		int m = row;
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
		
		BufferedWriter bw = new BufferedWriter(new FileWriter(new File(outdir, "rectangle_" + column+"x"+row+".neighbors.txt")));
		for(short i = 0 ; i < m*n ; i++) {
			bw.write(i+" ");
			short[] nexs = neighbors.get(i);
			for(int j = 0 ; j < nexs.length-1 ; j++) {
				bw.write(nexs[j]+",");
			}
			bw.write(nexs[nexs.length-1]+"\n");
		}
		bw.flush();
		bw.close();
		
	}
	
	public int indexNeighbor(File neighobrfile) throws IOException {
		neighbors = new HashMap<Short, short[]>(109);
		
		BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(neighobrfile)));
		String line = br.readLine();
		int linecnt = 0;
		while(line != null) {
			ArrayList<Short> nes = new ArrayList<Short>(3);
			short cur_tile = Short.parseShort(line.split(" ")[0]);
			String[] temps = line.split(" ")[1].split(",");
			for(int j = 0 ; j < temps.length ; j++) {
				nes.add(Short.parseShort(temps[j]));
			}
			short[] thisnes = new short[nes.size()];
			for(int k = 0 ; k < thisnes.length ; k++) {
				thisnes[k] = nes.get(k);
			}
			neighbors.put(cur_tile, thisnes);
			line = br.readLine();
			linecnt++;
		}
		return(linecnt);
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
		bw.write("");
		for(int i = 0 ; i < k.length-1 ; i++) {
			bw.write(""+k[i]);
			bw.write(" ");
		}
		bw.write(""+k[k.length-1]);
		bw.write("\n");
	}
	
	
	public void generateRectangleTiling(int column, int row, File neighborfile, File outdir) throws IOException {
		int numoftiles = this.indexNeighbor(neighborfile);
		File out1 = new File(outdir,"rectangle_"+column+"x"+row+"_partition.txt");
		File out2 = new File(outdir,"rectangle_"+column+"x"+row+"_tiling_fnctions.txt");
		File out3 = new File(outdir,"rectangle_"+column+"x"+row+"_equal_tiling.txt");
		core(numoftiles, out1,out2,out3);
	}
	
	public void generateDiamondTiling(int u, int v, File neighborfile, File outdir) throws IOException {
		int numoftiles = this.indexNeighbor(neighborfile);
		
		File out1 = new File(outdir,"diamond_"+u+"x"+v+"_partition.txt");
		File out2 = new File(outdir,"diamond_"+u+"x"+v+"_tiling_fnctions.txt");
		File out3 = new File(outdir,"diamond_"+u+"x"+v+"_equal_tiling.txt");
		core(numoftiles, out1,out2,out3);
	}
	
	public void generateHexagonTiling(double u, File neighborfile, File outdir) throws IOException {
		int numoftiles = this.indexNeighbor(neighborfile);
		
		File out1 = new File(outdir,"hexagon"+u+"_partition.txt");
		File out2 = new File(outdir,"hexagon"+u+"_tiling_fnctions.txt");
		File out3 = new File(outdir,"hexagon"+u+"_equal_tiling.txt");
		core(numoftiles, out1,out2,out3);
	}
	
	public void generateEllipseTiling(int u, File neighborfile, File outdir) throws IOException {
		int numoftiles = this.indexNeighbor(neighborfile);
		
		File out1 = new File(outdir,"ellipse"+u+"_partition.txt");
		File out2 = new File(outdir,"ellipse"+u+"_tiling_fnctions.txt");
		File out3 = new File(outdir,"ellipse"+u+"_equal_tiling.txt");
		core(numoftiles, out1,out2,out3);
	}
	
	private void core(int numoftiles, File out1, File out2, File out3) throws IOException {
		short[][] ini =initializePartition(numoftiles);
		short[] k = ini[0];
		short[] m = ini[1];
		long sum = 1;
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(out1));
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(out2));
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(out3));
		this.write(bw1, k);
		this.write(bw2, k);
		this.write(bw3, k);
		while(k != null) {
			if(sum == 405) 
				sum=405;
			k = this.nextPartition(k,m);
			if(k!= null )
			{
				this.write(bw1, k);
				sum++;
				if(this.adjacent(k)) {
					this.write(bw2, k);
					if(this.sameSize(k)) {
						this.write(bw3, k);
					}
				}
			} 
					
			if(sum%10000000 ==0)
				System.out.println(sum);
		
		}
		bw1.flush();
		bw1.close();
		bw2.flush();
		bw2.close();
		bw3.flush();
		bw3.close();
		System.out.println("Found " +  sum +" different tilings");
	}
	
	protected void coreEqual(int numoftiles, File out1, File out2, File out3) throws IOException {
		short[][] ini =initializePartition(numoftiles);
		short[] k = ini[0];
		short[] m = ini[1];
		long sum = 1;
		BufferedWriter bw1 = new BufferedWriter(new FileWriter(out1));
		BufferedWriter bw2 = new BufferedWriter(new FileWriter(out2));
		BufferedWriter bw3 = new BufferedWriter(new FileWriter(out3));
		this.write(bw1, k);
		this.write(bw2, k);
		this.write(bw3, k);
		while(k != null) {
			k = this.nextPartition(k,m);
			if(k!= null )
			{
				this.write(bw1, k);
				sum++;
				if(this.sameSize(k)) {
					if(this.adjacent(k)) {
						this.write(bw3, k);
					}
				}
			} 
					
			if(sum%10000000 ==0)
				System.out.println(sum);
		
		}
		bw1.flush();
		bw1.close();
		bw2.flush();
		bw2.close();
		bw3.flush();
		bw3.close();
		System.out.println("Found " +  sum +" different tilings");
	}
	
	
	public static void main(String args[]) throws Exception {
		TilingFunGen worker = new TilingFunGen();
		worker.generateEllipseTiling(8, new File("G:\\tilingfun\\ellipse_8_neighbors.txt"), new File("G:\\tilingfun"));
	}
}
