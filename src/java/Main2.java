import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigInteger;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiConsumer;
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
 */
final class Main2 {
    public static void main( String[] args ) throws IOException {
        var words = new BufferedReader( new FileReader(
                args.length > 0 ? args[ 0 ] : "tests/words.txt", US_ASCII ) ).lines();

        var encoder = new PhoneNumberEncoder2( words );

        new BufferedReader( new FileReader(
                args.length > 0 ? args[ 0 ] : "tests/numbers.txt", US_ASCII )
        ).lines().forEach( num -> encoder.encode( num,
                ( phone, solution ) -> System.out.println( phone + ": " + solution ) ) );
    }
}

final class PhoneNumberEncoder2 {
    private final Map<BigInteger, List<String>> dict;

    PhoneNumberEncoder2( Stream<String> words ) {
        dict = loadDictionary( words );
    }

    void encode( String phone, BiConsumer<String, String> onSolution ) {
        printTranslations( phone, removeIfNotLetterOrDigit( phone.toCharArray() ), 0, List.of(), onSolution );
    }

    private char[] removeIfNotLetterOrDigit( char[] chars ) {
        var finalLength = 0;
        for ( char c : chars ) {
            if ( Character.isLetterOrDigit( c ) ) finalLength++;
        }
        char[] result = new char[ finalLength ];
        var i = 0;
        for ( char c : chars ) {
            if ( Character.isLetterOrDigit( c ) ) {
                result[ i ] = c;
                i++;
            }
        }
        return result;
    }

    private void printTranslations( String num, char[] digits, int start, List<String> words, BiConsumer<String, String> onSolution ) {
        if ( start >= digits.length ) {
            onSolution.accept( num, String.join( " ", words ) );
            return;
        }
        final var foundWord = new AtomicBoolean( false );
        var n = BigInteger.ONE;
        for ( int i = start; i < digits.length; i++ ) {
            final var j = i;
            n = n.multiply( BigInteger.TEN ).add( BigInteger.valueOf( nthDigit( digits, i ) ) );
            Optional.ofNullable( dict.get( n ) ).ifPresent( foundWords -> {
                foundWord.set( true );
                for ( String word : foundWords ) {
                    printTranslations( num, digits, j + 1, append( words, word ), onSolution );
                }
            } );
        }
        if ( !foundWord.get() && !isLastItemDigit( words ) ) {
            printTranslations( num, digits, start + 1,
                    append( words, Integer.toString( nthDigit( digits, start ) ) ), onSolution );
        }
    }

    private static Map<BigInteger, List<String>> loadDictionary( Stream<String> words ) {
        var table = new HashMap<BigInteger, List<String>>( 100 );
        words.forEach( word -> table.computeIfAbsent( wordToNumber( word ),
                ( ignore ) -> new ArrayList<>() ).add( word ) );
        return table;
    }

    private boolean isLastItemDigit( List<String> words ) {
        return !words.isEmpty() && words.get( words.size() - 1 ).matches( "[0-9]" );
    }

    private static BigInteger wordToNumber( String word ) {
        var n = BigInteger.ONE;
        for ( char c : word.toCharArray() ) {
            if ( Character.isLetter( c ) ) {
                n = n.multiply( BigInteger.TEN ).add( BigInteger.valueOf( charToDigit( c ) ) );
            }
        }
        return n;
    }

    private static int nthDigit( char[] digits, int n ) {
        return digits[ n ] - ( ( int ) '0' );
    }

    static int charToDigit( char c ) {
        return switch ( Character.toLowerCase( c ) ) {
            case 'e' -> 0;
            case 'j', 'n', 'q' -> 1;
            case 'r', 'w', 'x' -> 2;
            case 'd', 's', 'y' -> 3;
            case 'f', 't' -> 4;
            case 'a', 'm' -> 5;
            case 'c', 'i', 'v' -> 6;
            case 'b', 'k', 'u' -> 7;
            case 'l', 'o', 'p' -> 8;
            case 'g', 'h', 'z' -> 9;
            default -> throw new RuntimeException( "Invalid char: " + c );
        };
    }

    static <T> List<T> append( List<T> list, T item ) {
        var result = new ArrayList<T>( list.size() + 1 );
        result.addAll( list );
        result.add( item );
        return Collections.unmodifiableList( result );
    }

}
