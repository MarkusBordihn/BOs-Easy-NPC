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
import java.util.Objects;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;

public class DialogButtonEntry {

  public static final String DATA_ACTIONS_TAG = "Actions";
  public static final String DATA_BUTTON_NAME_TAG = "Name";
  public static final String DATA_LABEL_TAG = "Label";
  public static final String DATA_TRANSLATE_TAG = "Translate";
  public static final String DATA_TYPE_TAG = "Type";
  public static final int MAX_BUTTON_LABEL_LENGTH = 32;
  private ActionDataSet actionDataSet;
  private UUID id;
  private String label = "";
  private String name;
  private boolean translate;
  private DialogButtonType type;

  public DialogButtonEntry(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public DialogButtonEntry(String name, String label, ActionDataSet actionDataSet) {
    this(name, label, DialogButtonType.DEFAULT, actionDataSet, false);
  }

  public DialogButtonEntry(String name, DialogButtonType type) {
    this(name, null, type, new ActionDataSet(), false);
  }

  public DialogButtonEntry(
      String name,
      String label,
      DialogButtonType type,
      ActionDataSet actionDataSet,
      boolean translate) {
    this.name = name;
    this.label = DialogUtils.generateButtonLabel(label != null && !label.isEmpty() ? label : name);
    this.id = UUID.nameUUIDFromBytes(this.label.getBytes());
    this.type = type;
    this.actionDataSet = actionDataSet != null ? actionDataSet : new ActionDataSet();
    this.translate = translate;
  }

  public UUID getId() {
    return this.id;
  }

  public String getLabel() {
    return this.label;
  }

  public void setLabel(String label) {
    this.label =
        DialogUtils.generateButtonLabel(label != null && !label.isEmpty() ? label : this.name);
    this.id = UUID.nameUUIDFromBytes(this.label.getBytes());
  }

  public String getName() {
    return this.name;
  }

  public void setName(String name) {
    this.name = name != null ? name : "";
  }

  public String getName(int maxLength) {
    return this.name.length() > maxLength ? this.name.substring(0, maxLength - 1) + '…' : this.name;
  }

  public Component getButtonName(int maxLength) {
    Component buttonName =
        this.translate ? Component.translatable(this.name) : Component.literal(this.name);
    if (buttonName.getString().length() > maxLength) {
      buttonName = Component.literal(buttonName.getString().substring(0, maxLength - 1) + '…');
    }
    return buttonName;
  }

  public DialogButtonType getType() {
    return this.type;
  }

  public boolean hasActionData() {
    return actionDataSet != null && actionDataSet.hasActionData();
  }

  public ActionDataSet getActionDataSet() {
    return this.actionDataSet;
  }

  public void setActionDataSet(ActionDataSet actionDataSet) {
    this.actionDataSet = actionDataSet == null ? new ActionDataSet() : actionDataSet;
  }

  public boolean getTranslate() {
    return this.translate;
  }

  public void load(CompoundTag compoundTag) {

    this.name = compoundTag.getString(DATA_BUTTON_NAME_TAG);
    this.type = DialogButtonType.get(compoundTag.getString(DATA_TYPE_TAG));
    this.translate =
        compoundTag.contains(DATA_TRANSLATE_TAG) && compoundTag.getBoolean(DATA_TRANSLATE_TAG);

    // Handle label and id creation
    if (compoundTag.contains(DATA_LABEL_TAG)) {
      this.setLabel(compoundTag.getString(DATA_LABEL_TAG));
    } else {
      this.setLabel(this.name);
    }

    // Load action data
    this.actionDataSet = new ActionDataSet(compoundTag, DATA_ACTIONS_TAG);
  }

  public CompoundTag save(CompoundTag compoundTag) {
    compoundTag.putString(DATA_BUTTON_NAME_TAG, this.name.trim());
    compoundTag.putString(DATA_TYPE_TAG, this.type.name());

    // Only save label if it is different from auto-generated label.
    if (!Objects.equals(DialogUtils.generateButtonLabel(name), this.label)) {
      compoundTag.putString(DATA_LABEL_TAG, this.label);
    }

    // Only save translate if it is true.
    if (this.translate) {
      compoundTag.putBoolean(DATA_TRANSLATE_TAG, true);
    }

    // Save action data
    this.actionDataSet.save(compoundTag, DATA_ACTIONS_TAG);

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
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
        + ", translate="
        + this.translate
        + ", actionDataSet="
        + this.actionDataSet
        + "]";
  }
}
