/* Name:

UID:

Others With Whom I Discussed Things:

Other Resources I Consulted:
    https://docs.oracle.com/javase/8/docs/api/java/util/stream/package-summary.html
    http://www.programcreek.com/2014/01/convert-stream-to-array-in-java-8/
    https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/RecursiveAction.html
    http://stackoverflow.com/questions/5785745/make-copy-of-array-java
    https://docs.oracle.com/javase/8/docs/api/java/util/stream/IntStream.html
    https://docs.oracle.com/javase/7/docs/api/java/lang/Long.html
*/

import java.io.*;
import java.util.Arrays;
import java.util.concurrent.*;
import java.util.stream.*;

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
                int avg = new Long(Math.round(.299 * pix.R + .587 * pix.G + .114 * pix.B)).intValue();
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
        RGB[] mirror = Arrays.copyOf(pixels, pixels.length);
        IntStream.range(0, height).parallel().forEach(curHeight -> {
            int left = curHeight * width;
            int right = ((curHeight + 1) * width) - 1;
            // Since we're mirroring the image, swap a column from the left half with one from the right half
            // If we went past width/2, then we would mirror the image and then mirror it again, resulting in the original image
            for (int i = 0; i < width/2; i++) {
                Helpers.swapPixels(mirror, left + i, right - i);
            }
        });
        return new PPMImage(width, height, maxColorVal, mirror);
    }

    // implement using Java's Fork/Join library
    public PPMImage gaussianBlur(int radius, double sigma) {
        RGB[] dest = new RGB[pixels.length];
        // Allocate and zero out the pixels of the blurred image
        for (int i = 0; i < dest.length; i++) {
            dest[i] = new RGB(0, 0, 0);
        }
        double[][] filter = Gaussian.gaussianFilter(radius, sigma);
        new GaussianTask(this.pixels, dest, filter, width, height, 0, height).compute();
        return new PPMImage(width, height, maxColorVal, dest);
    }
}

class Helpers {
    protected static void swapPixels(RGB[] pixels, int x, int y) {
        RGB temp = pixels[x];
        pixels[x] = pixels[y];
        pixels[y] = temp;
    }

    // Clamps value to range [min, max]
    protected static int clamp(int cur, int min, int max) {
        if (cur < min)
            return min;
        else if (cur > max)
            return max;
        return cur;
    }

    // Applies given Gaussian filter to the dest (x, y)
    protected static void applyGaussian(RGB[] source, RGB[] dest, double[][] filter, int x, int y, int width, int height) {
        int mid = filter.length / 2;
        double R = 0;
        double G = 0;
        double B = 0;

        for (int i = 0; i < filter.length; i++) {
            int clampX = clamp(x - (mid - i), 0, width - 1);
            for (int j = 0; j < filter[i].length; j++) {
                int clampY = clamp(y - (mid - j), 0, height - 1);
                R += filter[i][j] * source[clampY * width + clampX].R;
                G += filter[i][j] * source[clampY * width + clampX].G;
                B += filter[i][j] * source[clampY * width + clampX].B;
            }
        }

        dest[y * width + x].R = new Long(Math.round(R)).intValue();
        dest[y * width + x].G = new Long(Math.round(G)).intValue();
        dest[y * width + x].B = new Long(Math.round(B)).intValue();
    }
}

class MirrorTask extends RecursiveAction {
    private final RGB[] pixels;
    private final int width;
    private final int minHeight;
    private final int maxHeight;
    private final int SEQUENTIAL_CUTOFF = 25000;

    public MirrorTask(RGB[] pixels, int width, int minHeight, int maxHeight) {
        this.pixels = pixels;
        this.width = width;
        this.minHeight = minHeight;
        this.maxHeight = maxHeight;
    }

    public void compute() {
        // Only want to process ceil(num_pixels/SEQUENTIAL_CUTOFF) * SEQUENTIAL_CUTOFF or less pixels in this thread
        if ((maxHeight - minHeight) * width > SEQUENTIAL_CUTOFF) {
            int mid = minHeight + (maxHeight - minHeight) / 2;
            // The right side is processed in a forked task
            MirrorTask left = new MirrorTask(pixels, width, minHeight, mid);
            MirrorTask right = new MirrorTask(pixels, width, mid, maxHeight);
            right.fork();
            left.compute();
            right.join();
        }
        else {
            for (int i = minHeight; i < maxHeight; i++) {
                int left = i * width;
                int right = ((i + 1) * width) - 1;
                // Since we're mirroring the image, swap a column from the left half with one from the right half
                // If we went past width/2, then we would mirror the image and then mirror it again, resulting in the original image
                for (int j = 0; j < width/2; j++) {
                    Helpers.swapPixels(pixels, left + j, right - j);
                }
            }
        }
    }
}

class GaussianTask extends RecursiveAction {
    private final RGB[] source;
    private final RGB[] dest;
    private final double[][] filter;
    private final int width;
    private final int height;
    private final int minHeight;
    private final int maxHeight;
    private final int SEQUENTIAL_CUTOFF = 5000;

    public GaussianTask(RGB[] source, RGB[] dest, double[][] filter, int width, int height, int minHeight, int maxHeight) {
        this.source = source;
        this.dest = dest;
        this.filter = filter;
        this.width = width;
        this.height = height;
        this.minHeight = minHeight;
        this.maxHeight = maxHeight;
    }

    public void compute() {
        // Same idea as MirrorTask
        if ((maxHeight - minHeight) * width > SEQUENTIAL_CUTOFF) {
            int mid = minHeight + (maxHeight - minHeight) / 2;
            GaussianTask left = new GaussianTask(source, dest, filter, width, height, minHeight, mid);
            GaussianTask right = new GaussianTask(source, dest, filter, width, height, mid, maxHeight);
            right.fork();
            left.compute();
            right.join();
        }
        else {
            for (int i = minHeight; i < maxHeight; i++) {
                for (int j = 0; j < width; j++) {
                    Helpers.applyGaussian(source, dest, filter, j, i, width, height);
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
