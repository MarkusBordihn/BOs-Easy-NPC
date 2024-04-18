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

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionType;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;

public class DialogButtonData {

  // Limits
  public static final int MAX_BUTTON_LABEL_LENGTH = 32;

  // Dialog Button Data Tags
  public static final String DATA_ACTIONS_TAG = "Actions";
  public static final String DATA_BUTTON_NAME_TAG = "Name";
  public static final String DATA_TRANSLATE_TAG = "Translate";
  public static final String DATA_TYPE_TAG = "Type";
  public static final String DATA_LABEL_TAG = "Label";

  // Dialog Button Data
  private Set<ActionDataEntry> actionDatumEntries = new LinkedHashSet<>();
  private UUID id;
  private String label = "";
  private DialogButtonType type;
  private String name;
  private boolean translate;

  public DialogButtonData(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public DialogButtonData(String name, String label, Set<ActionDataEntry> actionDatumEntries) {
    this(name, label, DialogButtonType.DEFAULT, actionDatumEntries, false);
  }

  public DialogButtonData(String name, DialogButtonType type) {
    this(name, null, type, new LinkedHashSet<>(), false);
  }

  public DialogButtonData(
      String name,
      String label,
      DialogButtonType type,
      Set<ActionDataEntry> actionDatumEntries,
      boolean translate) {
    this.name = name;
    this.label = DialogUtils.generateButtonLabel(label != null && !label.isEmpty() ? label : name);
    this.id = UUID.nameUUIDFromBytes(this.label.getBytes());
    this.type = type;
    this.actionDatumEntries =
        actionDatumEntries != null ? actionDatumEntries : new LinkedHashSet<>();
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
        this.translate ? new TranslatableComponent(this.name) : new TextComponent(this.name);
    if (buttonName.getString().length() > maxLength) {
      buttonName = new TextComponent(buttonName.getString().substring(0, maxLength - 1) + '…');
    }
    return buttonName;
  }

  public DialogButtonType getType() {
    return this.type;
  }

  public boolean hasActionData() {
    if (this.actionDatumEntries == null || this.actionDatumEntries.isEmpty()) {
      return false;
    }
    for (ActionDataEntry action : this.actionDatumEntries) {
      if (action.isValidAndNotEmpty()) {
        return true;
      }
    }
    return false;
  }

  public Set<ActionDataEntry> getActionData() {
    return this.actionDatumEntries;
  }

  public void setActionData(Set<ActionDataEntry> actionDatumEntries) {
    this.actionDatumEntries =
        actionDatumEntries == null ? new LinkedHashSet<>() : actionDatumEntries;
  }

  public Set<ActionDataEntry> getAllActionData(ActionType actionType) {
    Set<ActionDataEntry> actionDataEntrySet = new LinkedHashSet<>();
    for (ActionDataEntry action : this.actionDatumEntries) {
      if (action.getType() == actionType) {
        actionDataEntrySet.add(action);
      }
    }
    return actionDataEntrySet;
  }

  public ActionDataEntry getActionData(ActionType actionType) {
    for (ActionDataEntry action : this.actionDatumEntries) {
      if (action.getType() == actionType) {
        return action;
      }
    }
    return null;
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
    ListTag actionDataList = compoundTag.getList(DATA_ACTIONS_TAG, 10);
    this.actionDatumEntries.clear();
    for (int i = 0; i < actionDataList.size(); i++) {
      this.actionDatumEntries.add(new ActionDataEntry(actionDataList.getCompound(i)));
    }
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
    ListTag actionDataList = new ListTag();
    for (ActionDataEntry action : this.actionDatumEntries) {
      actionDataList.add(action.save(new CompoundTag()));
    }
    compoundTag.put(DATA_ACTIONS_TAG, actionDataList);

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
        + ", actionData="
        + this.actionDatumEntries
        + "]";
  }
}
