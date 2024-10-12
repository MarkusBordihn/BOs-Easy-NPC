/*
 * Copyright 2022 Markus Bordihn
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

import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.minecraft.Util;
import net.minecraft.network.chat.Component;

public class TextFormattingCodes {

  private static final Map<String, String> TEXT_COLOR_CODES =
      Util.make(
          new HashMap<>(),
          map -> {
            // Color codes
            map.put("black", "§0");
            map.put("dark_blue", "§1");
            map.put("dark_green", "§2");
            map.put("dark_aqua", "§3");
            map.put("dark_red", "§4");
            map.put("dark_purple", "§5");
            map.put("gold", "§6");
            map.put("gray", "§7");
            map.put("dark_gray", "§8");
            map.put("blue", "§9");
            map.put("green", "§a");
            map.put("aqua", "§b");
            map.put("red", "§c");
            map.put("light_purple", "§d");
            map.put("yellow", "§e");
            map.put("white", "§f");
          });

  private static final Map<String, String> TEXT_FORMATTING_CODES =
      Util.make(
          new HashMap<>(),
          map -> {
            // Formatting codes
            map.put("obfuscated", "§k");
            map.put("bold", "§l");
            map.put("strikethrough", "§m");
            map.put("underline", "§n");
            map.put("italic", "§o");
            map.put("reset", "§r");

            // Short codes
            map.put("b", "§l");
            map.put("i", "§o");
            map.put("u", "§n");
            map.put("s", "§m");
          });

  private static final Set<String> textLinebreakCodes = new HashSet<>(List.of("<br>", "\\n"));

  private static final String FORMATTING_RESET_CODE = "§r";
  private static final String COLOR_DEFAULT_CODE = "§0";
  private static final String LINE_BREAK = "\n";

  private TextFormattingCodes() {}

  public static boolean hasTextFormattingCodes(String text) {
    return text != null && text.contains("<") && text.contains(">");
  }

  public static String parseTextFormattingCodes(String text) {
    if (!hasTextFormattingCodes(text)) {
      return text;
    }

    // Replace color codes
    for (Map.Entry<String, String> entry : TEXT_COLOR_CODES.entrySet()) {
      text = text.replace("<" + entry.getKey() + ">", entry.getValue());
      text = text.replace("</" + entry.getKey() + ">", COLOR_DEFAULT_CODE);
    }

    // Replace formatting codes
    for (Map.Entry<String, String> entry : TEXT_FORMATTING_CODES.entrySet()) {
      text = text.replace("<" + entry.getKey() + ">", entry.getValue());
      text = text.replace("</" + entry.getKey() + ">", FORMATTING_RESET_CODE);
    }

    return text;
  }

  public static boolean hasTextLinebreakCodes(Component component) {
    return component != null && hasTextLinebreakCodes(component.getString());
  }

  public static boolean hasTextLinebreakCodes(String text) {
    return text != null && !text.isEmpty() && textLinebreakCodes.stream().anyMatch(text::contains);
  }

  public static Component parseTextLineBreaks(Component component) {
    return component != null
        ? TextComponent.getText(parseTextLineBreaks(component.getString()))
        : null;
  }

  public static String parseTextLineBreaks(String text) {
    if (!hasTextLinebreakCodes(text)) {
      return text;
    }

    // Replace line break codes
    for (String lineBreakCode : textLinebreakCodes) {
      text = text.replace(lineBreakCode, LINE_BREAK);
    }
    return text;
  }
}
