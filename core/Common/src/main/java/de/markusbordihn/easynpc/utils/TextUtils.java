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

import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.security.CommandPermissionLevel;
import de.markusbordihn.easynpc.security.CommandSecurity;
import java.util.Locale;
import java.util.regex.Pattern;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.ClickEvent;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.HoverEvent;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.phys.Vec3;

public class TextUtils {

  private static final String TRANSLATION_KEY_REGEXP = "^[\\w-]+(?:\\.[\\w-]+)*\\.[\\w-]+$";
  private static final Pattern TRANSLATION_KEY_PATTERN = Pattern.compile(TRANSLATION_KEY_REGEXP);

  private TextUtils() {}

  public static boolean isTranslationKey(String text) {
    return text != null && !text.isEmpty() && TRANSLATION_KEY_PATTERN.matcher(text).matches();
  }

  public static String formatPosition(Vec3 position) {
    if (position == null) {
      return "";
    }

    return BlockPos.containing(position).toShortString();
  }

  public static Component formatTeleportPosition(Vec3 position, ServerPlayer serverPlayer) {
    if (position == null) {
      return TextComponent.getBlankText();
    }

    return formatTeleportPosition(BlockPos.containing(position), serverPlayer);
  }

  public static Component formatTeleportPosition(BlockPos blockPos, ServerPlayer serverPlayer) {
    if (blockPos == null) {
      return TextComponent.getBlankText();
    }

    MutableComponent positionText = TextComponent.getText(blockPos.toShortString());
    if (!CommandSecurity.getPlayerPermissionLevel(serverPlayer)
        .allows(CommandPermissionLevel.GAMEMASTERS)) {
      return positionText;
    }

    String teleportCommand =
        "/tp @s " + blockPos.getX() + " " + blockPos.getY() + " " + blockPos.getZ();
    return positionText.withStyle(
        style ->
            style
                .withColor(ChatFormatting.GREEN)
                .withUnderlined(true)
                .withClickEvent(new ClickEvent(ClickEvent.Action.SUGGEST_COMMAND, teleportCommand))
                .withHoverEvent(
                    new HoverEvent(
                        HoverEvent.Action.SHOW_TEXT,
                        Component.translatable("chat.coordinates.tooltip"))));
  }

  public static Component normalizeName(String name) {
    return TextComponent.getText(normalizeString(name));
  }

  public static String normalizeString(String string) {
    String normalizedString = string.toLowerCase(Locale.ROOT).replace("_", " ").replace("-", " ");
    normalizedString =
        normalizedString.substring(0, 1).toUpperCase(Locale.ROOT) + normalizedString.substring(1);
    return normalizedString;
  }

  public static String normalizeString(String string, int maxSize) {
    return limitString(normalizeString(string), maxSize);
  }

  public static String limitString(String string, int maxSize) {
    if (string == null || string.isBlank()) {
      return string;
    }
    String trimmedString = string.trim();
    int stringLength = trimmedString.length();
    if (stringLength <= maxSize) {
      return trimmedString;
    }
    return trimmedString.substring(0, maxSize) + '…';
  }

  public static Component removeAction(Component component) {
    MutableComponent mutableComponent =
        component.plainCopy().setStyle(component.getStyle().withClickEvent(null));
    for (Component componentSibling : component.getSiblings()) {
      mutableComponent.append(removeAction(componentSibling));
    }
    return mutableComponent;
  }

  public static String convertToPascalCase(String text) {
    if (text == null || text.isEmpty()) {
      return text;
    }
    return text.substring(0, 1).toUpperCase(Locale.ROOT) + convertToCamelCase(text.substring(1));
  }

  public static String convertToCamelCase(String text) {
    if (text == null || text.isEmpty()) {
      return text;
    }
    StringBuilder stringBuilder = new StringBuilder();
    boolean nextUpperCase = false;
    for (char character : text.toCharArray()) {
      if (character == '_' || character == ' ' || character == '-') {
        nextUpperCase = true;
      } else if (nextUpperCase) {
        stringBuilder.append(Character.toUpperCase(character));
        nextUpperCase = false;
      } else {
        stringBuilder.append(Character.toLowerCase(character));
      }
    }
    return stringBuilder.toString();
  }
}
