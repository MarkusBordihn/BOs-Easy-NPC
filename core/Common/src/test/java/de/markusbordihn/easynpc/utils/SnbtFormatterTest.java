/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.utils;

import static org.junit.jupiter.api.Assertions.*;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.TagParser;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class SnbtFormatterTest {

  @Test
  void testNullInput() {
    assertNull(SnbtFormatter.format(null));
  }

  @Test
  void testEmptyString() {
    assertEquals("", SnbtFormatter.format(""));
  }

  @Test
  void testSimpleCompound() {
    String input = "{name:\"Test\",value:123}";
    String expected = "{\n  name:\"Test\",\n  value:123\n}";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  void testNestedCompound() {
    String input = "{outer:{inner:\"value\"}}";
    String expected = "{\n  outer:{\n    inner:\"value\"\n  }\n}";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  void testArrays() {
    String input = "{items:[1,2,3]}";
    // Arrays are not formatted, only compound tags
    String expected = "{\n  items:[1,2,3]\n}";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  void testStringsWithSpecialCharacters() {
    String input = "{text:\"Hello, World!\"}";
    String expected = "{\n  text:\"Hello, World!\"\n}";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  void testEscapedQuotes() {
    String input = "{text:\"Say \\\"Hello\\\"\"}";
    String expected = "{\n  text:\"Say \\\"Hello\\\"\"\n}";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  void testEmptyCompound() {
    String input = "{empty:{}}";
    String expected = "{\n  empty:{}\n}";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  void testEmptyArray() {
    String input = "{items:[]}";
    String expected = "{\n  items:[]\n}";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  @DisplayName("Should preserve UUIDs and binary data")
  void testUUIDs() {
    String input = "{UUID:[I;123,456,789,012]}";
    String expected = "{\n  UUID:[I;123,456,789,012]\n}";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  void testComplexStructure() {
    String input = "{data:{name:\"Test\",items:[{id:1},{id:2}]}}";
    String expected =
        """
        {
          data:{
            name:"Test",
            items:[{
              id:1
            },{
              id:2
            }]
          }
        }""";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  void testMultipleFields() {
    String input = "{a:1,b:2,c:3,d:4}";
    String expected = "{\n  a:1,\n  b:2,\n  c:3,\n  d:4\n}";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  void testStripWhitespace() {
    String input = "{ a : 1 , b : 2 }";
    String expected = "{\n   a : 1 ,\n   b : 2 \n}";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  void testPreserveWhitespaceInStrings() {
    String input = "{text:\"  spaces  \"}";
    String expected = "{\n  text:\"  spaces  \"\n}";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  void testNewlinesInStrings() {
    String input = "{text:\"Line1\\nLine2\"}";
    String expected = "{\n  text:\"Line1\\nLine2\"\n}";
    assertEquals(expected, SnbtFormatter.format(input));
  }

  @Test
  @DisplayName("Should format real preset file and remain parseable")
  void testRealPresetFile() throws IOException, CommandSyntaxException {
    Path presetPath =
        Paths.get("src/test/resources/de/markusbordihn/easynpc/utils/test_preset.npc.snbt");
    if (!Files.exists(presetPath)) {
      fail("Test SNBT file not found: " + presetPath);
    }

    // Read and parse original file
    String originalSnbt = Files.readString(presetPath);
    CompoundTag originalTag = TagParser.parseTag(originalSnbt);

    String formattedSnbt = SnbtFormatter.format(originalSnbt);
    CompoundTag formattedTag = TagParser.parseTag(formattedSnbt);

    assertEquals(originalTag, formattedTag);

    assertTrue(formattedSnbt.contains("{\n"), "Opening braces should be followed by newline");
    assertTrue(formattedSnbt.contains("\n}"), "Closing braces should be on new line");

    assertNotEquals(
        originalSnbt,
        formattedSnbt,
        "Formatted content should be different from original single-line format");

    assertFalse(
        originalSnbt.contains("{\n  "),
        "Original file should not already be formatted with indentation");
  }
}
