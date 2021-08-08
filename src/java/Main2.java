import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Predicate;
import java.util.stream.Stream;

import static java.nio.charset.StandardCharsets.US_ASCII;

/**
 * This is a Java port of the Norvig's Common Lisp program from http://norvig.com/java-lisp.html.
 * <p>
 * The only relevant deviations from the original are:
 * <ul>
 *     <li>
 *     The words for a solution are accumulated using {@code cons} in CL, but using {@code List.add} in Java.
 *     This means that when a solution is found, the Java program doesn't need to reverse the words before printing
 *     them. Also, the Java program checks the <b>last</b> word when deciding if the previous word in a solution was
 *     a digit, while the CL program checks the <b>first</b> word.
 *     </li>
 *     <li>
 *     The {@code printTranslations} method in Java actually does not directly print the solutions,
 *     but takes a {@link BiConsumer} as an argument, which is called with each (number, solution) found.
 *     This was done to make the code more easily testable.
 *     </li>
 * </ul>
 *
 * @author Renato Athaydes
 */
final class Main2 {
    public static void main( String[] args ) throws IOException, InterruptedException {
        var words = new BufferedReader( new FileReader(
                args.length > 0 ? args[ 0 ] : "tests/words.txt", US_ASCII ) ).lines();


        var printer = new BufferedWriter( new OutputStreamWriter( System.out, US_ASCII ) );
        var filter = new InterleavingDigitsAndWordsOfSameLengthPrinterFilter2();

        try ( printer ) {
            var encoder = new PhoneNumberEncoder2( words, printer, filter );
            new BufferedReader( new FileReader(
                    args.length > 1 ? args[ 1 ] : "tests/numbers.txt", US_ASCII )
            ).lines().forEach( encoder::encode );
        }

        System.err.println( "Found solutions: " + filter.acceptedCount() +
                ", rejected: " + filter.rejectedSolutionCount() );
    }
}

interface PrinterFilter2 extends Predicate<List<String>> {
}

final class InterleavingDigitsAndWordsOfSameLengthPrinterFilter2 implements PrinterFilter2 {
    private int acceptedSolutions = 0;
    private long rejectedSolutions = 0;


    /**
     * A solution can only be printed if all words (non-digits) in it have the exact same length
     * and they interleave with digits.
     * <p>
     * For example, these are all valid solutions:
     * <ul>
     *     <li>abc</li>
     *     <li>abc 1</li>
     *     <li>abc 1 def 2 ghi</li>
     *     <li>1 abc</li>
     *     <li>1 abc 2</li>
     * </ul>
     *
     * @param solution current solution
     * @return whether this solution can be printed
     */
    @Override
    public boolean test( List<String> solution ) {
        if ( solution.size() == 0 ) {
            rejectedSolutions++;
            return false;
        }
        if ( solution.size() == 1 ) {
            acceptedSolutions++;
            return true;
        }
        var iterator = solution.iterator();
        var item = iterator.next();
        var wasDigit = PhoneNumberEncoder2.isDigit( item );
        var acceptableLength = wasDigit ? -1 : item.length();
        while ( iterator.hasNext() ) {
            item = iterator.next();
            var isDigit = PhoneNumberEncoder2.isDigit( item );
            if ( acceptableLength < 0 ) {
                // if the first item was a digit we get here...
                // we will return false if the current item is also a digit,
                // so we know this length can be safely used for all non-digits.
                acceptableLength = item.length();
            }
            // both previous and current are words or digits
            if ( wasDigit == isDigit
                    // not a digit but a word with different length
                    || ( !isDigit && item.length() != acceptableLength ) ) {
                rejectedSolutions++;
                return false;
            }
            wasDigit = isDigit;
        }
        acceptedSolutions++;
        return true;
    }

    public int acceptedCount() {
        return acceptedSolutions;
    }

    public long rejectedSolutionCount() {
        return rejectedSolutions;
    }
}

final class PhoneNumberEncoder2 {
    private final Map<ByteBuffer, List<String>> dict;
    private final Writer writer;
    private final PrinterFilter2 filter;

    PhoneNumberEncoder2( Stream<String> words, Writer writer, PrinterFilter2 filter ) {
        this.writer = writer;
        dict = loadDictionary( words );
        this.filter = filter;
    }

    void encode( String phone ) {
        printTranslations( phone, removeIfNotLetterOrDigit( phone.toCharArray() ), 0, new ArrayList<>() );
    }

    private byte[] removeIfNotLetterOrDigit( char[] chars ) {
        var finalLength = 0;
        for ( char c : chars ) {
            if ( Character.isLetterOrDigit( c ) ) finalLength++;
        }
        byte[] result = new byte[ finalLength ];
        var i = 0;
        for ( char c : chars ) {
            if ( Character.isLetterOrDigit( c ) ) {
                result[ i ] = ( byte ) c;
                i++;
            }
        }
        return result;
    }

    private void printTranslations( String num, byte[] digits, int start, List<String> words ) {
        if ( start >= digits.length ) {
            printSolution( num, words );
            return;
        }
        var foundWord = false;
        var bytes = ByteBuffer.allocate( digits.length - start );
        toReadState( bytes );
        for ( int i = start; i < digits.length; i++ ) {
            bytes = toWriteState( bytes ).put( nthDigit( digits, i ) );
            List<String> foundWords = dict.get( toReadState( bytes ) );
            if ( foundWords != null ) {
                foundWord = true;
                for ( String word : foundWords ) {
                    words.add( word );
                    printTranslations( num, digits, i + 1, words );
                    words.remove( words.size() - 1 );
                }
            }
        }
        if ( !foundWord && !isLastItemDigit( words ) ) {
            words.add( Integer.toString( nthDigit( digits, start ) ) );
            printTranslations( num, digits, start + 1, words );
            words.remove( words.size() - 1 );
        }
    }

    private void printSolution( String num, List<String> words ) {
        if ( filter.test( words ) ) {
            // writing it out by invoking println several times like Rust does is counter-productive
            try {
                writer.write( num + ": " + String.join( " ", words ) );
                writer.write( '\n' );
            } catch ( IOException e ) {
                throw new RuntimeException( e );
            }
        }
    }

    private static Map<ByteBuffer, List<String>> loadDictionary( Stream<String> words ) {
        var table = new HashMap<ByteBuffer, List<String>>( 100 );
        words.forEach( word -> table.computeIfAbsent( wordToNumber( word ),
                ( ignore ) -> new ArrayList<>() ).add( word ) );
        return table;
    }

    private boolean isLastItemDigit( List<String> words ) {
        if ( words.isEmpty() ) return false;
        var lastWord = words.get( words.size() - 1 );
        return isDigit( lastWord );
    }

    static boolean isDigit( String word ) {
        return word.length() == 1 && Character.isDigit( word.chars().sum() );
    }

    private static ByteBuffer wordToNumber( String word ) {
        ByteBuffer bytes = ByteBuffer.allocate( word.length() );
        for ( char c : word.toCharArray() ) {
            if ( Character.isLetter( c ) ) {
                bytes.put( charToDigit( c ) );
            }
        }
        return toReadState( bytes );
    }

    private static ByteBuffer toReadState( ByteBuffer bytes ) {
        bytes.limit( bytes.position() );
        bytes.position( 0 );
        return bytes;
    }

    private static ByteBuffer toWriteState( ByteBuffer bytes ) {
        var pos = bytes.limit();
        bytes.limit( bytes.capacity() );
        bytes.position( pos );
        return bytes;
    }

    private static byte nthDigit( byte[] digits, int n ) {
        return ( byte ) ( digits[ n ] - ( byte ) '0' );
    }

    static byte charToDigit( char c ) {
        return switch ( Character.toLowerCase( c ) ) {
            case 'e' -> ( byte ) 0;
            case 'j', 'n', 'q' -> ( byte ) 1;
            case 'r', 'w', 'x' -> ( byte ) 2;
            case 'd', 's', 'y' -> ( byte ) 3;
            case 'f', 't' -> ( byte ) 4;
            case 'a', 'm' -> ( byte ) 5;
            case 'c', 'i', 'v' -> ( byte ) 6;
            case 'b', 'k', 'u' -> ( byte ) 7;
            case 'l', 'o', 'p' -> ( byte ) 8;
            case 'g', 'h', 'z' -> ( byte ) 9;
            default -> throw new RuntimeException( "Invalid char: " + c );
        };
    }

}
