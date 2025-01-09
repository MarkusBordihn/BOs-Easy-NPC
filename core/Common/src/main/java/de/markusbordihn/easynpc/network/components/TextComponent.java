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

package de.markusbordihn.easynpc.network.components;

import de.markusbordihn.easynpc.Constants;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;

public class TextComponent {

  private TextComponent() {}

  public static MutableComponent getTextComponentRaw(final String text, final boolean translate) {
    if (translate) {
      return getTranslatedTextRaw(text);
    }
    return getText(text);
  }

  public static MutableComponent getTextComponent(final String text) {
    if (text != null && !text.isBlank() && Character.isLowerCase(text.codePointAt(0))) {
      return getTranslatedConfigText(text);
    }
    return getText(text != null ? text : "");
  }

  public static MutableComponent getTextComponent(final String text, final Player player) {
    return getTextComponent(text, player.getDisplayName());
  }

  public static MutableComponent getTextComponent(final String text, final String data) {
    if (text != null && !text.isBlank() && Character.isLowerCase(text.codePointAt(0))) {
      return getTranslatedConfigText(text, data);
    }
    return getText(text != null ? text : "");
  }

  public static MutableComponent getTextComponent(final String text, final Component data) {
    if (text != null && !text.isBlank() && Character.isLowerCase(text.codePointAt(0))) {
      return getTranslatedConfigText(text, data);
    }
    return getText(text != null ? text : "");
  }

  public static MutableComponent getTranslatedText(final String translationKey) {
    return getTranslatedTextRaw(Constants.TEXT_PREFIX + translationKey);
  }

  public static MutableComponent getTranslatedText(final String translationKey, final String data) {
    return getTranslatedTextRaw(Constants.TEXT_PREFIX + translationKey, data);
  }

  public static MutableComponent getTranslatedText(
      final String translationKey, final String data, final String data2) {
    return getTranslatedTextRaw(Constants.TEXT_PREFIX + translationKey, data, data2);
  }

  public static MutableComponent getTranslatedText(
      final String translationKey, final Component data) {
    return getTranslatedTextRaw(Constants.TEXT_PREFIX + translationKey, data);
  }

  public static MutableComponent getTranslatedText(
      final String translationKey, final LivingEntity livingEntity, final Player player) {
    return getTranslatedTextRaw(
        Constants.TEXT_PREFIX + translationKey,
        livingEntity.getDisplayName(),
        player.getDisplayName());
  }

  public static MutableComponent getTranslatedConfigText(final String translationKey) {
    return getTranslatedTextRaw(Constants.TEXT_CONFIG_PREFIX + translationKey);
  }

  public static MutableComponent getTranslatedConfigText(
      final String translationKey, final String data) {
    return getTranslatedTextRaw(Constants.TEXT_CONFIG_PREFIX + translationKey, data);
  }

  public static MutableComponent getTranslatedConfigText(
      final String translationKey, final String data, final String data2) {
    return getTranslatedTextRaw(Constants.TEXT_CONFIG_PREFIX + translationKey, data, data2);
  }

  public static MutableComponent getTranslatedConfigText(
      final String translationKey, final Component data) {
    return getTranslatedTextRaw(Constants.TEXT_CONFIG_PREFIX + translationKey, data);
  }

  public static MutableComponent getTranslatedTextRaw(final String translationKey) {
    return new TranslatableComponent(translationKey);
  }

  public static MutableComponent getTranslatedTextRaw(
      final String translationKey, final String data) {
    return new TranslatableComponent(translationKey, data);
  }

  public static MutableComponent getTranslatedTextRaw(
      final String translationKey, final String data, final String data2) {
    return new TranslatableComponent(translationKey, data, data2);
  }

  public static MutableComponent getTranslatedTextRaw(
      final String translationKey, final Component data) {
    return new TranslatableComponent(translationKey, data);
  }

  public static MutableComponent getTranslatedTextRaw(
      final String translationKey, final Component data, final Component data2) {
    return new TranslatableComponent(translationKey, data, data2);
  }

  public static MutableComponent getText(final String text) {
    return new net.minecraft.network.chat.TextComponent(text);
  }

  public static MutableComponent getBlankText() {
    return new net.minecraft.network.chat.TextComponent("");
  }
}
