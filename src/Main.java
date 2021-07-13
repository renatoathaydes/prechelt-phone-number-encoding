import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.nio.charset.StandardCharsets.US_ASCII;

final class Main {
    public static void main( String[] args ) throws IOException {
        var words = new InputParser( WordsInputCleaner::clean )
                .parse( new File( "words.txt" ) )
                .collect( Collectors.toSet() );

        var encoder = new PhoneNumberEncoder( words );

        new InputParser( PhoneNumberCleaner::clean )
                .parse( new File( "numbers.txt" ) )
                .forEach( phone -> encoder.encode( phone ).forEach( System.out::println ) );
    }

}

/*
The following mapping from letters to digits is given:

E | J N Q | R W X | D S Y | F T | A M | C I V | B K U | L O P | G H Z
e | j n q | r w x | d s y | f t | a m | c i v | b k u | l o p | g h z
0 |   1   |   2   |   3   |  4  |  5  |   6   |   7   |   8   |   9

For the encoding only the letters are used, but
the words must be printed in exactly the form given in the dictionary.
Leading non-letters do not occur in the dictionary.

Encodings of phone numbers can consist of a single word or of multiple
words separated by spaces. The encodings are built word by word from
left to right. If and only if at a particular point no word at all from
the dictionary can be inserted, a single digit from the phone number can
be copied to the encoding instead. Two subsequent digits are never
allowed, though. To put it differently: In a partial encoding that
currently covers k digits, digit k+1 is encoded by itself if and only if,
first, digit k was not encoded by a digit and, second, there is no word
in the dictionary that can be used in the encoding starting at digit k+1.
 */
final class PhoneNumberEncoder {
    private final Trie dictionary;

    PhoneNumberEncoder( Set<Item> words ) {
        dictionary = new Trie();
        for ( Item word : words ) {
            dictionary.put( word );
        }
    }

    Set<Item> encode( Item phone ) {
        return dictionary.get( phone );
    }
}

final class Trie {
    // A node is an item in a List representing a current candidate for a solution...
    // A full solution is a List of Node's where each Node is a word or a char digit
    static class Node {
        final Item item;
        final int digit;

        Node( Item item ) {
            assert item != null;
            this.item = item;
            this.digit = -1;
        }

        Node( int digit ) {
            this.item = null;
            this.digit = digit;
        }

        boolean isDigit() {
            return item == null;
        }

        static Item toItem( List<Node> nodes, Item phone ) {
            var solution = nodes.stream()
                    .map( node -> node.item == null ? node.digit : node.item.original )
                    .map( Object::toString )
                    .collect( Collectors.joining( " " ) );
            return new Item( phone.original, solution );
        }
    }

    final Trie[] items = new Trie[ 10 ];
    final List<Item> values = new ArrayList<>( 2 );
    final Trie root;

    Trie() {
        root = this;
    }

    Trie( Trie root ) {
        this.root = root;
    }

    void put( Item item ) {
        put( item.result.toCharArray(), 0, item );
    }

    void put( char[] chars, int index, Item item ) {
        if ( index < chars.length ) {
            var digit = charToDigit( chars[ index ] );
            Trie current = items[ digit ];
            if ( current == null ) {
                current = new Trie( root );
                items[ digit ] = current;
            }
            current.put( chars, index + 1, item );
        } else {
            values.add( item );
        }
    }

    Set<Item> get( Item phone ) {
        char[] chars = phone.result.toCharArray();
        var result = getRecurse( List.of(), chars, 0 );
        return result.stream().map( n -> Node.toItem( n, phone ) ).collect( Collectors.toSet() );
    }

    private List<List<Node>> getRecurse( List<Node> solution, char[] chars, int index ) {
        if ( index < chars.length ) {
            var digit = chars[ index ] - 48;
            var trie = items[ digit ];
            if ( trie != null ) {
                if ( trie.values.isEmpty() ) {
                    // This is an intermediate trie, keep going
                    var result = trie.getRecurse( solution, chars, index + 1 );
                    if ( result.isEmpty() ) {
                        result = tryInjectDigitIfAllowed( solution, chars, index );
                    }
                    return result;
                } else {
                    // each word in this trie may provide a new solution
                    List<List<Node>> solutions = new ArrayList<>( 2 );
                    for ( Item word : trie.values ) {
                        solutions.addAll( findMoreWords( append( solution, new Node( word ) ), chars, index + 1 ) );
                    }
                    // and there may be longer words that can provide solutions as well
                    solutions.addAll( trie.getRecurse( solution, chars, index + 1 ) );
                    return solutions;
                }
            } else if ( index == 0 ) {
                return tryInjectDigitIfAllowed( solution, chars, index );
            }
        }
        return List.of();
    }

    private List<List<Node>> tryInjectDigitIfAllowed( List<Node> solution, char[] chars, int index ) {
        // If and only if at a particular point no word at all from
        // the dictionary can be inserted, a single digit from the phone number can
        // be copied to the encoding instead. Two subsequent digits are never allowed, though.
        var lastItemWasDigit = !solution.isEmpty() && solution.get( solution.size() - 1 ).isDigit();
        if ( !lastItemWasDigit ) {
            return findMoreWords( append( solution, new Node( chars[ index ] - 48 ) ), chars, index + 1 );
        }
        return List.of();
    }

    private List<List<Node>> findMoreWords( List<Node> solution, char[] chars, int index ) {
        if ( index < chars.length ) {
            var next = new char[ chars.length - index ];
            System.arraycopy( chars, index, next, 0, next.length );
            return root.getRecurse( solution, next, 0 );
        }
        // done navigating the phone number
        return List.of( solution );
    }

//    E | J N Q | R W X | D S Y | F T | A M | C I V | B K U | L O P | G H Z
//    e | j n q | r w x | d s y | f t | a m | c i v | b k u | l o p | g h z
//    0 |   1   |   2   |   3   |  4  |  5  |   6   |   7   |   8   |   9
    static int charToDigit( char c ) {
        return switch ( c ) {
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

final class Item {
    final String original;
    final String result;

    Item( String original, String result ) {
        this.original = original;
        this.result = result;
    }

    Item( String result ) {
        this.original = result;
        this.result = result;
    }

    @Override
    public boolean equals( Object o ) {
        if ( this == o ) return true;
        if ( o == null || getClass() != o.getClass() ) return false;

        Item word = ( Item ) o;

        if ( !original.equals( word.original ) ) return false;
        return result.equals( word.result );
    }

    @Override
    public int hashCode() {
        int result = original.hashCode();
        result = 31 * result + this.result.hashCode();
        return result;
    }

    @Override
    public String toString() {
        return original + ": " + result;
    }
}

final class InputParser {
    private final Function<String, String> cleaner;

    public InputParser( Function<String, String> cleaner ) {
        this.cleaner = cleaner;
    }

    Stream<Item> parse( File file ) throws IOException {
        return parse( new BufferedReader( new FileReader( file, US_ASCII ) ) );
    }

    Stream<Item> parse( BufferedReader reader ) {
        return reader.lines()
                .map( line -> new Item( line, cleaner.apply( line ) ) )
                .filter( word -> !word.result.isEmpty() );
    }

}

/*
The words are taken from a dictionary which
is given as an alphabetically sorted ASCII file (one word per line).

[NOTE: The dictionary is in German and contains umlaut characters
encoded as double-quotes.  The double-quotes should be ignored.  EG.]
 */
final class WordsInputCleaner {
    private static final Pattern NOT_LETTERS = Pattern.compile( "[^a-zA-Z]" );

    static String clean( String word ) {
        return NOT_LETTERS.matcher( word ).replaceAll( "" ).toLowerCase( Locale.ENGLISH );
    }
}

/*
A phone number is an
arbitrary(!) string of dashes - , slashes / and digits. The dashes and
slashes will not be encoded.
 */
final class PhoneNumberCleaner {
    private static final Pattern IGNORE_CHARS = Pattern.compile( "[-/]" );

    static String clean( String phone ) {
        return IGNORE_CHARS.matcher( phone ).replaceAll( "" );
    }
}
