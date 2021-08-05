import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class InterleavingDigitsAndWordsOfSameLengthPrinterFilterTest {

    static Trie.Node node( int n ) {
        return new Trie.Node( n );
    }

    static Trie.Node node( String word ) {
        return new Trie.Node( new Item( word ) );
    }

    static Stream<List<Trie.Node>> goodExamples() {
        return Stream.of(
                List.of( node( 4 ) ),
                List.of( node( 4 ), node( "foo" ) ),
                List.of( node( 4 ), node( "foo" ), node( 3 ) ),
                List.of( node( "foo" ) ),
                List.of( node( "foo" ), node( 5 ) ),
                List.of( node( "foo" ), node( 5 ), node( "bar" ) ),
                List.of( node( "foo" ), node( 5 ), node( "bar" ), node( 6 ) ),
                List.of( node( 6 ), node( "foo" ), node( 5 ), node( "bar" ), node( 6 ) )
        );
    }

    static Stream<List<Trie.Node>> badExamples() {
        return Stream.of(
                List.of(),
                List.of( node( 4 ), node( 5 ) ),
                List.of( node( 4 ), node( "foo" ), node( "bar" ) ),
                List.of( node( 4 ), node( "foo" ), node( "z" ) ),
                List.of( node( 4 ), node( "foo" ), node( 3 ), node( "z" ) ),
                List.of( node( "foo" ), node( 5 ), node( 6 ) ),
                List.of( node( "foo" ), node( 5 ), node( "z" ) ),
                List.of( node( "z" ), node( 5 ), node( "foo" ) ),
                List.of( node( "foo" ), node( 5 ), node( "z" ), node( 6 ) ),
                List.of( node( 6 ), node( "foo" ), node( 5 ), node( "z" ), node( 6 ) )
        );
    }

    final PrinterFilter filter = new InterleavingDigitsAndWordsOfSameLengthPrinterFilter();

    @ParameterizedTest
    @MethodSource( "goodExamples" )
    void filtersInExpectedEntries( List<Trie.Node> example ) {
        assertTrue( filter.test( example ) );
    }

    @ParameterizedTest
    @MethodSource( "badExamples" )
    void filtersOutExpectedEntries( List<Trie.Node> example ) {
        assertFalse( filter.test( example ) );
    }
}
