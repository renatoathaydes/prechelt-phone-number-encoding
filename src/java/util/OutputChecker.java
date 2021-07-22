package util;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static java.nio.charset.StandardCharsets.US_ASCII;

/**
 * Takes two files as arguments:
 * - dictionary file
 * - program output
 * Checks that every solution printed matches the following conditions:
 * - matches the format 'phone: solution'.
 * - each word in the solution is either a digit or a word from the dictionary.
 * - no two subsequent words in a solution are a digit.
 * - after removing the redundant characters from the solution, each character of the solution
 * can encode the respective digit of the phone number, or is the same digit.
 * - for each phone number, any solution that contains a digit has no other solution where, in the same position,
 * a word could fit.
 * <p>
 * There's one condition that is currently missing, which is the case where a valid solution is missing, but to
 * know that it would be necessary to have a provably correct program that could be used as an oracle.
 *
 * @author Renato Athaydes
 */
public class OutputChecker {
    private static final Pattern IRRELEVANT_SOLUTION_CHARS = Pattern.compile( "[^0-9a-zA-Z]" );

    public static void main( String[] args ) throws IOException {
        var dictionary = new File( args[ 0 ] );
        var solutions = new File( args[ 1 ] );

        var phoneSolutions = new PhoneSolutions();
        var words = new HashSet<>( Files.readAllLines( dictionary.toPath(), US_ASCII ) );
        var lineNumber = new AtomicInteger( 0 );
        Files.readAllLines( solutions.toPath(), US_ASCII ).forEach( line -> {
            lineNumber.incrementAndGet();
            var parts = line.split( ":\\s" );
            if ( parts.length != 2 ) {
                fail( lineNumber, "not in format <phone: solution>: '" + line + "'" );
            }
            check( parts[ 0 ], parts[ 1 ], words, lineNumber, phoneSolutions );
        } );

        // verify last phone solutions
        verify( phoneSolutions, lineNumber );

        System.out.println( "OK" );
    }

    private static void check( String phone, String solution, HashSet<String> words,
                               AtomicInteger lineNumber, PhoneSolutions phoneSolutions ) {
        var solutionParts = solution.split( " " );
        for ( String part : solutionParts ) {
            if ( !isDigit( part ) && !words.contains( part ) ) {
                fail( lineNumber, "Solution contains word '" + part + "' which is not in dictionary: " + solution );
            }
        }
        var cleanPhone = phone.replaceAll( "[-/]", "" );
        var cleanSolution = cleanSolution( solution );
        if ( cleanPhone.length() != cleanSolution.length() ) {
            fail( lineNumber, "Lengths differ: phone='" + cleanPhone + "', solution='" + cleanSolution + "'" );
        }
        if ( !phoneSolutions.phone.equals( cleanPhone ) ) {
            verify( phoneSolutions, lineNumber );
            phoneSolutions.setNewPhone( cleanPhone );
        }
        phoneSolutions.solutionsParts.add( Arrays.stream( solutionParts )
                .map( OutputChecker::cleanSolution )
                .toArray( String[]::new ) );

        var phoneChars = cleanPhone.toCharArray();
        var solutionChars = cleanSolution.toCharArray();

        for ( int i = 0; i < phoneChars.length; i++ ) {
            var phoneChar = phoneChars[ i ];
            var solutionChar = solutionChars[ i ];
            if ( !canEncode( phoneChar, solutionChar ) ) {
                fail( lineNumber, "Unexpected char in solution. Digit " + phoneChar +
                        " cannot be encoded as '" + solutionChar + "'" );
            }
        }
    }

    private static void verify( PhoneSolutions phoneSolutions, AtomicInteger lineNumber ) {
        var allSolutions = phoneSolutions.solutionsParts;
        var digitIndexes = new boolean[ 50 ];
        for ( String[] parts : allSolutions ) {
            var prevWasDigit = false;
            var index = 0;
            for ( String part : parts ) {
                var partIsDigit = isDigit( part );
                if ( partIsDigit ) {
                    if ( prevWasDigit ) {
                        fail( lineNumber, "Phone '" + phoneSolutions.phone + "' has solution containing " +
                                "two sub-sequent digits: '" + String.join( " ", parts ) + "'" );
                    }
                    digitIndexes[ index ] = true;
                }
                prevWasDigit = partIsDigit;
                index += part.length();
            }
        }

        // check that each solution has no word starting at an index where there was also a digit
        for ( String[] parts : allSolutions ) {
            var index = 0;
            for ( String part : parts ) {
                if ( !isDigit( part ) && digitIndexes[ index ] ) { // it's a word but there's a digit also
                    fail( lineNumber, "Phone '" + phoneSolutions.phone + "' has solutions containing " +
                            "digit at index " + index + ", but this solution has a word at this index: '" +
                            String.join( " ", parts ) + "'. Other solutions were:\n" +
                            allSolutions.stream().filter( s -> s != parts )
                                    .map( s -> "  * " + String.join( " ", s ) )
                                    .collect( Collectors.joining( "\n" ) ) );
                }
                index += part.length();
            }
        }

    }

    private static String cleanSolution( String solution ) {
        return IRRELEVANT_SOLUTION_CHARS.matcher( solution ).replaceAll( "" ).toLowerCase( Locale.ENGLISH );
    }

    private static boolean isDigit( String solution ) {
        return solution.length() == 1 && Character.isDigit( solution.charAt( 0 ) );
    }

    static boolean canEncode( char digit, char solution ) {
        //    e | j n q | r w x | d s y | f t | a m | c i v | b k u | l o p | g h z
        //    0 |   1   |   2   |   3   |  4  |  5  |   6   |   7   |   8   |   9
        return switch ( digit ) {
            case '0' -> solution == '0' || solution == 'e';
            case '1' -> solution == '1' || solution == 'j' || solution == 'n' || solution == 'q';
            case '2' -> solution == '2' || solution == 'r' || solution == 'w' || solution == 'x';
            case '3' -> solution == '3' || solution == 'd' || solution == 's' || solution == 'y';
            case '4' -> solution == '4' || solution == 'f' || solution == 't';
            case '5' -> solution == '5' || solution == 'a' || solution == 'm';
            case '6' -> solution == '6' || solution == 'c' || solution == 'i' || solution == 'v';
            case '7' -> solution == '7' || solution == 'b' || solution == 'k' || solution == 'u';
            case '8' -> solution == '8' || solution == 'l' || solution == 'o' || solution == 'p';
            case '9' -> solution == '9' || solution == 'g' || solution == 'h' || solution == 'z';
            default -> throw new RuntimeException( "Phone contains non-digit: '" + digit + "'" );
        };
    }

    private static void fail( AtomicInteger lineNumber, String reason ) {
        throw new RuntimeException( "ERROR at line " + lineNumber.get() + ": " + reason );
    }
}

class PhoneSolutions {
    String phone = "";
    List<String[]> solutionsParts = new ArrayList<>( 2 );

    void setNewPhone( String phone ) {
        this.phone = phone;
        solutionsParts = new ArrayList<>( 2 );
    }
}
