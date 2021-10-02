import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import static java.nio.charset.StandardCharsets.US_ASCII;

/**
 * My second solution to the problem.
 * 
 * @author David Alvarez Fidalgo
 */
public class Faster {

	public static void main(String[] args) {
		// long t1 = System.currentTimeMillis();

		BufferedWriter printer = new BufferedWriter(new OutputStreamWriter(System.out, US_ASCII));
		PhoneEncoder pps = new PhoneEncoder(args.length > 0 ? args[0] : "tests/words.txt", printer);
		List<String> phoneNumbers = loadPhoneNumbers(args.length > 1 ? args[1] : "tests/numbers.txt");

		try (printer) {
			for (String number : phoneNumbers)
				pps.encode(number);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}

		// System.out.printf("Time: %d ms\n", System.currentTimeMillis() - t1);
	}

	/**
	 * Loads the phone numbers from a file
	 * 
	 * @param fileName name of the file
	 * @return list of phone numbers
	 */
	private static List<String> loadPhoneNumbers(String fileName) {
		BufferedReader file = null;
		List<String> phoneNumbers = new ArrayList<>();

		try {
			file = new BufferedReader(new FileReader(fileName));
			while (file.ready()) {
				phoneNumbers.add(file.readLine().trim());
			}
		} catch (FileNotFoundException e) {
			System.out.println("File not found: " + fileName);
		} catch (IOException e) {
			System.out.println("Error reading from file: " + fileName);
		}

		return phoneNumbers;
	}

	/**
	 * Implements the solution to the problem
	 * 
	 * @author David Alvarez Fidalgo
	 */
	private static class PhoneEncoder {

		private WordTree dict;
		private String phoneNumberStr;
		private List<Integer> phoneNumber;
		private int maxWordLength;
		private int minWordLength;
		private BufferedWriter printer;

		/**
		 * Creates an instance of the encoder and loads the words from a dictionary file
		 * 
		 * @param dictionaryFileName name of the file containing the words
		 */
		public PhoneEncoder(String dictionaryFileName, BufferedWriter printer) {
			this.printer = printer;
			loadDict(dictionaryFileName);
		}

		/**
		 * Encodes the phone number
		 * 
		 * @param phoneNumberStr string containing the phone number
		 */
		public void encode(String phoneNumberStr) {
			this.phoneNumberStr = phoneNumberStr;
			loadPhoneNumber(phoneNumberStr);

			// If the phone number has only one digit and the shortest word has more than
			// one letter,
			// the only solution is the digit itself
			if (phoneNumber.size() == 1 && minWordLength > 1) {
				try {
					printer.write(phoneNumberStr + ": " + phoneNumber.get(0).toString());
					printer.write('\n');
				} catch (IOException e) {
                    throw new RuntimeException( e );
                }
				return;
			}

			// If the phone number is empty or the phone number has more than one digits but
			// less than the
			// length of the shortest word, there is no solution
			if (phoneNumber.size() == 0 || (phoneNumber.size() > 1 && phoneNumber.size() < minWordLength)) {
				return;
			}

			encodeRec(0, new ArrayList<>(), false);
		}

		/**
		 * Loads the words from a file
		 * 
		 * @param dictionaryFileName name of the file containing the words
		 */
		private void loadDict(String dictionaryFileName) {
			BufferedReader file = null;
			dict = new WordTree(0);
			maxWordLength = 0;
			minWordLength = Integer.MAX_VALUE;

			try {
				file = new BufferedReader(new FileReader(dictionaryFileName));
				String line;

				while (file.ready()) {
					line = file.readLine().trim();
					dict.add(line);
					if (line.length() > maxWordLength)
						maxWordLength = line.length();
					if (line.length() < minWordLength)
						minWordLength = line.length();
				}
			} catch (FileNotFoundException e) {
				System.out.println("File not found: " + dictionaryFileName);
			} catch (IOException e) {
				System.out.println("Error reading from file: " + dictionaryFileName);
			}
		}

		/**
		 * Converts the string containing the phone number into a list of integers
		 * 
		 * @param phoneNumberStr string containing the phone number
		 */
		private void loadPhoneNumber(String phoneNumberStr) {
			phoneNumber = new ArrayList<>();
			for (int i = 0; i < phoneNumberStr.length(); i++) {
				if (Character.isDigit(phoneNumberStr.charAt(i))) {
					phoneNumber.add((int) phoneNumberStr.charAt(i) - 48);
				}
			}
		}

		/**
		 * Recursive method that builds the solution. We reduce the number of steps
		 * needed to encode a phone number by grouping words by its length. The partial
		 * solution is an irregular matrix that holds a sequence of list with words of
		 * the same length. E. g. if 123 translates to both "foo" and "bar" and the
		 * number to encode is 12345, we can group "foo" and "bar", and then encode 45
		 * only one time, instead of having to do it twice.
		 * 
		 * @param digitPos     position of the current phone number digit
		 * @param partSolution current partial solution
		 * @param lastWasDigit whether the last string added to the partial solution was
		 *                     a digit or not
		 */
		private void encodeRec(int digitPos, List<List<String>> partSolution, boolean lastWasDigit) {
			Map<Integer, List<String>> words = dict.findWords(phoneNumber, digitPos);
			boolean wordsFound = !words.isEmpty();

			// If no word was found, we add the digit to the solution
			if (!wordsFound) {
				// We can't have two consecutive digits in the solution
				if (lastWasDigit)
					return;
				List<String> wordList = new ArrayList<>();
				wordList.add(Integer.toString(phoneNumber.get(digitPos)));
				words.put(1, wordList);
			}

			for (Map.Entry<Integer, List<String>> entry : words.entrySet()) {
				partSolution.add(entry.getValue());
				if (digitPos + entry.getKey() == phoneNumber.size()) {
					printSolution(partSolution);
				} else {
					encodeRec(digitPos + entry.getKey(), partSolution, !wordsFound);
				}
				partSolution.remove(partSolution.size() - 1);
			}
		}

		/**
		 * Transforms an irregular matrix containing the encodings of a phone number
		 * into a list of solutions
		 * 
		 * @param solutionMatrix irregular matrix containing the encodings
		 * @return list of solutions
		 */
		private void printSolution(List<List<String>> solutionMatrix) {
			printSolutionRec(0, new ArrayList<>(), solutionMatrix);
		}

		/**
		 * Recursively transforms an irregular matrix containing the encodings of a
		 * phone number into a list of solutions
		 * 
		 * @param i              next x index in the matrix
		 * @param partSolution   current partial solution
		 * @param solutionMatrix irregular matrix containing the encodings of a phone
		 *                       number
		 * @param solutionsList  list of solutions
		 */
		private void printSolutionRec(int i, List<String> partSolution, List<List<String>> solutionMatrix) {
			if (i == solutionMatrix.size()) {
				try {
					printer.write(phoneNumberStr + ": " + joinWordList(partSolution));
					printer.write('\n');
				} catch (IOException e) {
                    throw new RuntimeException( e );
                }
				return;
			}

			for (String word : solutionMatrix.get(i)) {
				partSolution.add(word);
				printSolutionRec(i + 1, partSolution, solutionMatrix);
				partSolution.remove(partSolution.size() - 1);
			}
		}

		/**
		 * Joins a list of words leaving spaces between them
		 * 
		 * @param wordList list of words
		 * @return string containing the list of words joined with spaces between them
		 */
		private String joinWordList(List<String> wordList) {
			StringBuilder sb = new StringBuilder(wordList.get(0));
			for (int i = 1; i < wordList.size(); i++) {
				sb.append(" " + wordList.get(i));
			}
			return sb.toString();
		}

		/**
		 * Represents a tree formed by nodes. Each node can have up to 10 children (one
		 * for each digit) and stores a list of words that can be encoded by the
		 * sequence of digits used to traverse the tree. E. g. the node that is reached
		 * by going from the root into the child corresponding to the digit "1" and the
		 * into the child corresponding to "2" contains the list of words that can be
		 * encoded with the number "12".
		 * 
		 * @author David Alvarez Fidalgo
		 */
		private static class WordTree {

			private static final Pattern REGEX = Pattern.compile("[^a-zA-Z]");

			private int depth;
			private WordTree[] children;
			private List<String> words;

			/**
			 * Creates a new tree at a certain depth
			 * 
			 * @param depth of the tree
			 */
			public WordTree(int depth) {
				this.depth = depth;
				this.children = new WordTree[10];
				this.words = new ArrayList<>();
			}

			/**
			 * Adds a word to the tree
			 * 
			 * @param word to add
			 */
			public void add(String word) {
				add(word, REGEX.matcher(word).replaceAll("").toLowerCase(), 0);
			}

			/**
			 * Recursively adds a word to the tree by selecting a child for each letter of
			 * the word
			 * 
			 * @param originalWord word to add
			 * @param cleanedWord  word without double quotes and in lower case
			 * @param letterPos    current letter in the word
			 */
			private void add(String originalWord, String cleanedWord, int letterPos) {
				// If we have used every letter from the word, we add it to the node's list
				if (letterPos == cleanedWord.length()) {
					words.add(originalWord);
					return;
				}

				int nextIndex = letterToDigit(cleanedWord.charAt(letterPos));
				if (children[nextIndex] == null) {
					children[nextIndex] = new WordTree(depth + 1);
				}
				children[nextIndex].add(originalWord, cleanedWord, letterPos + 1);
			}

			/**
			 * Finds all the words that can be formed starting from a digit of the phone
			 * number
			 * 
			 * @param digitPos digit of the phone number
			 * @return list of words found, grouped by length
			 */
			public Map<Integer, List<String>> findWords(List<Integer> phoneNumber, int digitPos) {
				Map<Integer, List<String>> words = new HashMap<>();
				findWordsRec(phoneNumber, digitPos, words);
				return words;
			}

			/**
			 * Traverses the tree using a digit of the phone number to select a child each
			 * time
			 * 
			 * @param phoneNumber phone number to be encoded
			 * @param digitPos    current digit of the phone number to be used
			 * @param words       list of words found, grouped by length
			 */
			private void findWordsRec(List<Integer> phoneNumber, int digitPos, Map<Integer, List<String>> words) {
				// If we have already used every digit, we return
				if (digitPos == phoneNumber.size()) {
					return;
				}

				WordTree child = children[phoneNumber.get(digitPos)];
				// If there is no children corresponding to the digit, no more words can be
				// added
				if (child == null) {
					return;
				}

				if (!child.words.isEmpty()) {
					words.put(depth + 1, child.words);
				}
				child.findWordsRec(phoneNumber, digitPos + 1, words);
			}

			/**
			 * Encodes a letter.
			 * 
			 * @param letter to encode
			 * @return digit corresponding to the letter
			 */
			private int letterToDigit(char letter) {
				// I use Java 13 in my development environment so this looks ugly af
				switch (letter) {
				case 'j':
				case 'n':
				case 'q':
					return 1;
				case 'r':
				case 'w':
				case 'x':
					return 2;
				case 'd':
				case 's':
				case 'y':
					return 3;
				case 'f':
				case 't':
					return 4;
				case 'a':
				case 'm':
					return 5;
				case 'c':
				case 'i':
				case 'v':
					return 6;
				case 'b':
				case 'k':
				case 'u':
					return 7;
				case 'l':
				case 'o':
				case 'p':
					return 8;
				case 'g':
				case 'h':
				case 'z':
					return 9;
				default:
					return 0;
				}
			}
		}

	}

}
