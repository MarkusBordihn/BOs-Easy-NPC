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

package de.markusbordihn.easynpc.data.dialog;

import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;

public class DialogTextData {

  public static final String DATA_TEXT_TAG = "Text";
  public static final String DATA_TRANSLATE_TAG = "Translate";

  private UUID id;
  private String text;
  private boolean translate;

  public DialogTextData(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public DialogTextData(String text) {
    this(text, false);
  }

  public DialogTextData(String text, boolean translate) {
    this.id = UUID.randomUUID();
    this.text = text != null ? text.trim() : "";
    this.translate = translate;
  }

  public UUID getId() {
    return this.id;
  }

  public String getText() {
    return this.text;
  }

  public void setText(String text) {
    this.text = text;
  }

  public String getText(int maxLength) {
    return this.text.length() > maxLength ? this.text.substring(0, maxLength - 1) + '…' : this.text;
  }

  public String getDialogText() {
    return this.translate
        ? new TranslatableComponent(this.text).getString()
        : new TextComponent(this.text).getString();
  }

  public String getDialogText(LivingEntity entity, Player player) {
    return DialogUtils.parseDialogText(getDialogText(), entity, player);
  }

  public boolean getTranslate() {
    return this.translate;
  }

  public void setTranslate(boolean translate) {
    this.translate = translate;
  }

  public void load(CompoundTag compoundTag) {
    this.text = compoundTag.getString(DATA_TEXT_TAG);
    this.translate =
        compoundTag.contains(DATA_TRANSLATE_TAG) && compoundTag.getBoolean(DATA_TRANSLATE_TAG);
  }

  public CompoundTag save(CompoundTag compoundTag) {
    compoundTag.putString(DATA_TEXT_TAG, this.text.trim());
    if (this.translate) {
      compoundTag.putBoolean(DATA_TRANSLATE_TAG, true);
    }
    return compoundTag;
  }
}
