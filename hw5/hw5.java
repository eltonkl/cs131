/* Name:

UID:

Others With Whom I Discussed Things:

Other Resources I Consulted:
    https://docs.oracle.com/javase/8/docs/api/java/util/stream/package-summary.html
    http://www.programcreek.com/2014/01/convert-stream-to-array-in-java-8/
    https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/RecursiveAction.html
    http://stackoverflow.com/questions/5785745/make-copy-of-array-java
*/

import java.io.*;
import java.util.Arrays;
import java.util.concurrent.*;

// a marker for code that you need to implement
class ImplementMe extends RuntimeException {}

// an RGB triple
class RGB {
    public int R, G, B;

    RGB(int r, int g, int b) {
        R = r;
        G = g;
        B = b;
    }

    public String toString() { return "(" + R + "," + G + "," + B + ")"; }

}


// an object representing a single PPM image
class PPMImage {
    protected int width, height, maxColorVal;
    protected RGB[] pixels;

    public PPMImage(int w, int h, int m, RGB[] p) {
        width = w;
        height = h;
        maxColorVal = m;
        pixels = p;
    }

    // parse a PPM image file named fname and produce a new PPMImage object
    public PPMImage(String fname) 
        throws FileNotFoundException, IOException {
        FileInputStream is = new FileInputStream(fname);
        BufferedReader br = new BufferedReader(new InputStreamReader(is));
        br.readLine(); // read the P6
        String[] dims = br.readLine().split(" "); // read width and height
        int width = Integer.parseInt(dims[0]);
        int height = Integer.parseInt(dims[1]);
        int max = Integer.parseInt(br.readLine()); // read max color value
        br.close();

        is = new FileInputStream(fname);
        // skip the first three lines
        int newlines = 0;
        while (newlines < 3) {
            int b = is.read();
            if (b == 10)
                newlines++;
        }

        int MASK = 0xff;
        int numpixels = width * height;
        byte[] bytes = new byte[numpixels * 3];
        is.read(bytes);
        RGB[] pixels = new RGB[numpixels];
        for (int i = 0; i < numpixels; i++) {
            int offset = i * 3;
            pixels[i] = new RGB(bytes[offset] & MASK, 
                    bytes[offset+1] & MASK, 
                    bytes[offset+2] & MASK);
        }
        is.close();

        this.width = width;
        this.height = height;
        this.maxColorVal = max;
        this.pixels = pixels;
    }

    // write a PPMImage object to a file named fname
    public void toFile(String fname) throws IOException {
        FileOutputStream os = new FileOutputStream(fname);

        String header = "P6\n" + width + " " + height + "\n" 
            + maxColorVal + "\n";
        os.write(header.getBytes());

        int numpixels = width * height;
        byte[] bytes = new byte[numpixels * 3];
        int i = 0;
        for (RGB rgb : pixels) {
            bytes[i] = (byte) rgb.R;
            bytes[i+1] = (byte) rgb.G;
            bytes[i+2] = (byte) rgb.B;
            i += 3;
        }
        os.write(bytes);
        os.close();
    }

    // implement using Java 8 Streams
    public PPMImage negate() {
        RGB[] newPixels = Arrays.stream(pixels).parallel()
            .map(pix -> new RGB(maxColorVal - pix.R, maxColorVal - pix.G, maxColorVal - pix.B))
            .toArray(RGB[]::new);
        return new PPMImage(width, height, maxColorVal, newPixels);
    }

    // implement using Java 8 Streams
    public PPMImage greyscale() {
        RGB[] newPixels = Arrays.stream(pixels).parallel()
            .map(pix -> {
                int avg = Math.round(.299f * pix.R + .587f * pix.G + .114f * pix.B);
                return new RGB(avg, avg, avg);
            }).toArray(RGB[]::new);
        return new PPMImage(width, height, maxColorVal, newPixels);
    }    

    // implement using Java's Fork/Join library
    public PPMImage mirrorImage() {
        PPMImage mirror = new PPMImage(width, height, maxColorVal, Arrays.copyOf(pixels, pixels.length));
        new MirrorTask(mirror.pixels, width, 0, height).compute();
        return mirror;
    }

    // implement using Java 8 Streams
    public PPMImage mirrorImage2() {
        throw new ImplementMe();
    }

    // implement using Java's Fork/Join library
    public PPMImage gaussianBlur(int radius, double sigma) {
        throw new ImplementMe();
    }

}

class Helpers {
    public static void swapPixels(RGB[] pixels, int x, int y) {
        RGB temp = pixels[x];
        pixels[x] = pixels[y];
        pixels[y] = temp;
    }
}

class MirrorTask extends RecursiveAction {
    private final RGB[] pixels;
    private final int width;
    private final int minHeight;
    private final int maxHeight;
    private final int SEQUENTIAL_CUTOFF = 10000;

    public MirrorTask(RGB[] pixels, int width, int minHeight, int maxHeight) {
        this.pixels = pixels;
        this.width = width;
        this.minHeight = minHeight;
        this.maxHeight = maxHeight;
    }

    public void compute() {
        if ((maxHeight - minHeight) * width > SEQUENTIAL_CUTOFF) {
            int mid = minHeight + (maxHeight - minHeight) / 2;
            MirrorTask left = new MirrorTask(pixels, width, minHeight, mid);
            MirrorTask right = new MirrorTask(pixels, width, mid, maxHeight);
            right.fork();
            left.compute();
            right.join();
        }
        else {
            for (int i = minHeight; i < maxHeight; i++) {
                for (int j = 0; j < width/2; j++) {
                    Helpers.swapPixels(pixels, (i * width) + j, (((i + 1) * width) - 1) - j);
                }
            }
        }
    }
}

// code for creating a Gaussian filter
class Gaussian {

    protected static double gaussian(int x, int mu, double sigma) {
        return Math.exp( -(Math.pow((x-mu)/sigma,2.0))/2.0 );
    }

    public static double[][] gaussianFilter(int radius, double sigma) {
        int length = 2 * radius + 1;
        double[] hkernel = new double[length];
        for(int i=0; i < length; i++)
            hkernel[i] = gaussian(i, radius, sigma);
        double[][] kernel2d = new double[length][length];
        double kernelsum = 0.0;
        for(int i=0; i < length; i++) {
            for(int j=0; j < length; j++) {
                double elem = hkernel[i] * hkernel[j];
                kernelsum += elem;
                kernel2d[i][j] = elem;
            }
        }
        for(int i=0; i < length; i++) {
            for(int j=0; j < length; j++)
                kernel2d[i][j] /= kernelsum;
        }
        return kernel2d;
    }
}
