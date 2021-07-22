package util;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Map;
import java.util.regex.Pattern;

import static java.util.Map.entry;

/**
 * Download the German words from
 * https://gist.githubusercontent.com/MarvinJWendt/2f4f4154b8ae218600eb091a5706b5f4/raw/36b70dd6be330aa61cd4d4cdfda6234dcb0b8784/wordlist-german.txt
 * into a file.
 * <p>
 * Run this class the the file path as argument.
 * <p>
 * The output is written to file {@code <input-file-name>.ascii}.
 *
 * @author Renato Athaydes
 */
public class CleanupWords {

    // do not transform any characters matching this pattern
    private static final Pattern ASCII_ALPHABET = Pattern.compile( "[a-zA-Z ]+" );

    // replace non-ASCII letters with a suitable ASCII replacement
    private static final Map<String, String> asciiReplacements = Map.ofEntries(
            entry( "Ä", "A\"" ),
            entry( "Å", "A'" ),
            entry( "É", "E'" ),
            entry( "Ö", "O\"" ),
            entry( "Ü", "U\"" ),
            entry( "ß", "B\"" ),
            entry( "à", "a\\" ),
            entry( "á", "a/" ),
            entry( "â", "a^" ),
            entry( "ã", "a~" ),
            entry( "ä", "a\"" ),
            entry( "å", "a0" ),
            entry( "æ", "ae" ),
            entry( "ç", "c," ),
            entry( "è", "e\\" ),
            entry( "é", "e/" ),
            entry( "ê", "e^" ),
            entry( "ë", "e\"" ),
            entry( "ì", "i\\" ),
            entry( "í", "i/" ),
            entry( "î", "i^" ),
            entry( "ï", "i\"" ),
            entry( "ñ", "n~" ),
            entry( "ò", "o\\" ),
            entry( "ó", "o/" ),
            entry( "ô", "o^" ),
            entry( "ö", "o\"" ),
            entry( "ø", "o|" ),
            entry( "ú", "u/" ),
            entry( "û", "u^" ),
            entry( "ü", "u\"" )
    );

    public static void main( String[] args ) throws IOException {
        if ( args.length == 0 ) throw new RuntimeException( "Please provide the dictionary file as an argument" );
        var input = new File( args[ 0 ] );
        var maxWords = args.length > 1 ? Integer.parseInt( args[ 1 ] ) : 75_000;
        var output = new File( input.getName() + ".ascii" );
        if ( output.getParentFile() != null ) {
            output.getParentFile().mkdirs();
        }
        var word = new StringBuilder( 16 );
        var character = new StringBuilder( 4 );
        var wordCount = 0;

        try ( var out = new FileWriter( output, StandardCharsets.UTF_8 ) ) {
            for ( String line : Files.readAllLines( input.toPath() ) ) {
                for ( int c : line.codePoints().toArray() ) {
                    character.appendCodePoint( c );
                    if ( ASCII_ALPHABET.matcher( character ).matches() ) {
                        word.appendCodePoint( c );
                    } else {
                        String replacement = asciiReplacements.get( character.toString() );
                        if ( replacement == null ) {
                            throw new RuntimeException( "No ASCII replacement for " + character );
                        }
                        word.append( replacement );
                    }
                    character.delete( 0, character.length() );
                }
                out.append( word ).append( '\n' );
                word.delete( 0, word.length() );

                wordCount++;

                if ( wordCount == maxWords ) {
                    break;
                }
            }
        }

    }
}
