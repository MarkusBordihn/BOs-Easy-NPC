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

import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.utils.TextUtils;
import de.markusbordihn.easynpc.utils.UUIDUtils;
import java.util.Objects;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;

public record DialogButtonEntry(
    UUID id,
    String name,
    String label,
    DialogButtonType type,
    ActionDataSet actionDataSet,
    boolean isTranslationKey) {

  public static final String DATA_ACTIONS_TAG = "Actions";
  public static final String DATA_BUTTON_NAME_TAG = "Name";
  public static final String DATA_LABEL_TAG = "Label";
  public static final String DATA_TYPE_TAG = "Type";
  public static final int MAX_BUTTON_LABEL_LENGTH = 32;

  public DialogButtonEntry(CompoundTag compoundTag) {
    this(
        compoundTag.getString(DATA_BUTTON_NAME_TAG),
        compoundTag.getString(DATA_LABEL_TAG),
        DialogButtonType.get(compoundTag.getString(DATA_TYPE_TAG)),
        new ActionDataSet(compoundTag, DATA_ACTIONS_TAG));
  }

  public DialogButtonEntry(String name, String label, ActionDataSet actionDataSet) {
    this(name, label, DialogButtonType.DEFAULT, actionDataSet);
  }

  public DialogButtonEntry(String name, DialogButtonType type) {
    this(name, null, type, new ActionDataSet());
  }

  public DialogButtonEntry(
      String name, String label, DialogButtonType type, ActionDataSet actionDataSet) {
    this(
        UUIDUtils.textToUUID(
            label != null && !label.isEmpty() ? label : DialogUtils.generateButtonLabel(name)),
        name,
        label != null && !label.isEmpty() ? label : DialogUtils.generateButtonLabel(name),
        type,
        actionDataSet != null ? actionDataSet : new ActionDataSet(),
        TextUtils.isTranslationKey(name));
  }

  public String name(int maxLength) {
    return this.name.length() > maxLength ? this.name.substring(0, maxLength - 1) + '…' : this.name;
  }

  public Component getButtonName(int maxLength) {
    Component buttonName =
        this.isTranslationKey ? new TranslatableComponent(this.name) : new TextComponent(this.name);
    if (buttonName.getString().length() > maxLength) {
      buttonName = new TextComponent(buttonName.getString().substring(0, maxLength - 1) + '…');
    }
    return buttonName;
  }

  public boolean hasActionData() {
    return actionDataSet != null && actionDataSet.hasActionData();
  }

  public DialogButtonEntry withName(String name) {
    return new DialogButtonEntry(
        this.id, name, this.label, this.type, this.actionDataSet, TextUtils.isTranslationKey(name));
  }

  public DialogButtonEntry withLabel(String label) {
    return new DialogButtonEntry(
        UUIDUtils.textToUUID(label != null && !label.isEmpty() ? label : name),
        this.name,
        label,
        this.type,
        this.actionDataSet,
        this.isTranslationKey);
  }

  public DialogButtonEntry withActionDataSet(ActionDataSet actionDataSet) {
    return new DialogButtonEntry(
        this.id,
        this.name,
        this.label,
        this.type,
        actionDataSet != null ? actionDataSet : new ActionDataSet(),
        this.isTranslationKey);
  }

  public DialogButtonEntry create(CompoundTag compoundTag) {
    return new DialogButtonEntry(compoundTag);
  }

  public CompoundTag write(CompoundTag compoundTag) {
    compoundTag.putString(DATA_BUTTON_NAME_TAG, this.name.trim());
    compoundTag.putString(DATA_TYPE_TAG, this.type.name());

    // Only save label if it is different from auto-generated label.
    if (this.label != null && !Objects.equals(DialogUtils.generateButtonLabel(name), this.label)) {
      compoundTag.putString(DATA_LABEL_TAG, this.label);
    }

    // Save action data
    this.actionDataSet.save(compoundTag, DATA_ACTIONS_TAG);

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.write(new CompoundTag());
  }

  @Override
  public String toString() {
    return "DialogButtonData [id="
        + this.id
        + ", name="
        + this.name
        + ", label="
        + this.label
        + ", type="
        + this.type
        + ", isTranslationKey="
        + this.isTranslationKey
        + ", actionDataSet="
        + this.actionDataSet
        + "]";
  }
}
