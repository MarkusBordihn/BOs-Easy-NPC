/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.security;

import de.markusbordihn.easynpc.Constants;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import net.minecraft.network.chat.Component;

public class PresetWarningMessages {

  private static final String NOTICE_PREFIX = Constants.TEXT_PREFIX + "preset.sanitize.";

  private PresetWarningMessages() {}

  public static List<Component> toPlayerMessages(PresetSanitizationResult result) {
    if (result == null || result.notices() == null || result.notices().isEmpty()) {
      return List.of();
    }

    Set<String> messageKeys = new LinkedHashSet<>();
    for (PresetSanitizationNotice notice : result.notices()) {
      messageKeys.add(getMessageKey(notice));
    }

    List<Component> messages = new ArrayList<>();
    for (String messageKey : messageKeys) {
      messages.add(Component.translatable(messageKey));
    }

    return messages;
  }

  public static String getMessageKey(PresetSanitizationNotice notice) {
    return NOTICE_PREFIX + notice.name().toLowerCase(Locale.ROOT);
  }
}
