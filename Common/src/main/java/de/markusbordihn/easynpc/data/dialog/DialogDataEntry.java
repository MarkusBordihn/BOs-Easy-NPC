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

import de.markusbordihn.easynpc.Constants;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.network.chat.Component;

public final class DialogDataEntry {

  public static final String DATA_BUTTONS_TAG = "Buttons";
  public static final String DATA_DIALOG_NAME = "Name";
  public static final String DATA_LABEL_TAG = "Label";
  public static final String DATA_TEXTS_TAG = "Texts";
  public static final String DATA_TEXT_TAG = "Text";
  public static final int MAX_DIALOG_LABEL_LENGTH = 32;
  private Set<DialogButtonEntry> dialogButtons = new LinkedHashSet<>();
  private Set<DialogTextData> dialogTexts = new LinkedHashSet<>();
  private UUID id;
  private String label = "";
  private String name;

  public DialogDataEntry(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public DialogDataEntry(String name) {
    this(null, name, "Dialog text", null);
  }

  public DialogDataEntry(String name, String text) {
    this("default", name, text, null);
  }

  public DialogDataEntry(String label, String name, String text) {
    this(label, name, text, null);
  }

  public DialogDataEntry(
      String label, String name, String text, Set<DialogButtonEntry> dialogButtons) {
    this.label = DialogUtils.generateButtonLabel(label != null && !label.isEmpty() ? label : name);
    this.id = UUID.nameUUIDFromBytes(this.label.getBytes());
    this.name = name != null ? name.trim() : this.label;
    this.dialogButtons = dialogButtons != null ? dialogButtons : new LinkedHashSet<>();
    this.dialogTexts.add(new DialogTextData(text));
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

  public String getLabel(int maxLength) {
    return this.label.length() > maxLength
        ? this.label.substring(0, maxLength - 1) + '…'
        : this.label;
  }

  public String getName() {
    return this.name;
  }

  public void setName(String name) {
    this.name = name != null ? name.trim() : "";
  }

  public String getName(int maxLength) {
    return this.name.length() > maxLength ? this.name.substring(0, maxLength - 1) + '…' : this.name;
  }

  public String getText() {
    return this.dialogTexts.iterator().next().text();
  }

  public void setText(String text) {
    this.dialogTexts.clear();
    this.dialogTexts.add(new DialogTextData(text));
  }

  public Component getDialogText() {
    if (this.dialogTexts == null || this.dialogTexts.isEmpty()) {
      return Constants.EMPTY_TEXT_COMPONENT;
    }

    // Return first dialog text or random dialog text.
    DialogTextData dialogTextData =
        this.dialogTexts.size() == 1
            ? this.dialogTexts.iterator().next()
            : this.dialogTexts.stream()
                .skip(ThreadLocalRandom.current().nextInt(this.dialogTexts.size()))
                .findFirst()
                .orElse(null);

    return dialogTextData != null ? dialogTextData.getDialogText() : Constants.EMPTY_TEXT_COMPONENT;
  }

  public String getText(int maxLength) {
    return this.dialogTexts.iterator().next().getText(maxLength);
  }

  public String getDialogText(DialogMetaData dialogMetaData) {
    return DialogUtils.parseDialogText(getDialogText(), dialogMetaData);
  }

  public Set<DialogTextData> getDialogTexts() {
    return this.dialogTexts;
  }

  public void setDialogTexts(Set<DialogTextData> dialogTexts) {
    this.dialogTexts = dialogTexts;
  }

  public Set<DialogButtonEntry> getDialogButtons() {
    return this.dialogButtons;
  }

  public void setDialogButtons(Set<DialogButtonEntry> buttons) {
    this.dialogButtons = buttons;
  }

  public DialogButtonEntry getDialogButton(UUID dialogButtonId) {
    for (DialogButtonEntry button : this.dialogButtons) {
      if (button.id().equals(dialogButtonId)) {
        return button;
      }
    }
    return null;
  }

  public DialogButtonEntry getDialogButton(String label) {
    for (DialogButtonEntry button : this.dialogButtons) {
      if (button.label().equals(label)) {
        return button;
      }
    }
    return null;
  }

  public void setDialogButton(DialogButtonEntry dialogButtonEntry) {
    this.setDialogButton(dialogButtonEntry.id(), dialogButtonEntry);
  }

  public void setDialogButton(UUID dialogButtonId, DialogButtonEntry dialogButtonEntry) {
    if (dialogButtonId != null) {
      for (DialogButtonEntry button : this.dialogButtons) {
        if (button.id().equals(dialogButtonId)) {
          this.dialogButtons.remove(button);
          this.dialogButtons.add(dialogButtonEntry);
          return;
        }
      }
    }
    this.dialogButtons.add(dialogButtonEntry);
  }

  public boolean hasDialogButton(String label) {
    for (DialogButtonEntry button : this.dialogButtons) {
      if (button.label().equals(label)) {
        return true;
      }
    }
    return false;
  }

  public boolean hasDialogButton(UUID dialogButtonId) {
    for (DialogButtonEntry button : this.dialogButtons) {
      if (button.id().equals(dialogButtonId)) {
        return true;
      }
    }
    return false;
  }

  public boolean removeDialogButton(UUID dialogButtonId) {
    for (DialogButtonEntry button : this.dialogButtons) {
      if (button.id().equals(dialogButtonId)) {
        this.dialogButtons.remove(button);
        return true;
      }
    }
    return false;
  }

  public int getNumberOfDialogButtons() {
    return this.dialogButtons.size();
  }

  public void load(CompoundTag compoundTag) {
    this.name = compoundTag.getString(DATA_DIALOG_NAME);

    // Handle label and id creation
    this.setLabel(
        compoundTag.contains(DATA_LABEL_TAG) ? compoundTag.getString(DATA_LABEL_TAG) : this.name);

    // Load dialog texts, if available.
    if (compoundTag.contains(DATA_TEXTS_TAG)) {
      this.dialogTexts.clear();
      ListTag dialogTextsList = compoundTag.getList(DATA_TEXTS_TAG, 10);
      if (!dialogTextsList.isEmpty()) {
        for (int i = 0; i < dialogTextsList.size(); i++) {
          this.dialogTexts.add(new DialogTextData(dialogTextsList.getCompound(i)));
        }
      }
    } else if (compoundTag.contains(DATA_TEXT_TAG)) {
      this.dialogTexts.clear();
      this.dialogTexts.add(new DialogTextData(compoundTag.getString(DATA_TEXT_TAG)));
    }

    // Load buttons, if available.
    if (compoundTag.contains(DATA_BUTTONS_TAG)) {
      this.dialogButtons.clear();
      ListTag buttonsList = compoundTag.getList(DATA_BUTTONS_TAG, 10);
      if (!buttonsList.isEmpty()) {
        for (int i = 0; i < buttonsList.size(); i++) {
          this.dialogButtons.add(new DialogButtonEntry(buttonsList.getCompound(i)));
        }
      }
    }
  }

  public CompoundTag save(CompoundTag compoundTag) {
    compoundTag.putString(DATA_DIALOG_NAME, this.name.trim());

    // Only save label if it is different from auto-generated label.
    if (!Objects.equals(DialogUtils.generateDialogLabel(name), this.label)) {
      compoundTag.putString(DATA_LABEL_TAG, this.label);
    }

    // Save dialog text
    if (this.dialogTexts != null && !this.dialogTexts.isEmpty()) {
      ListTag dialogTextsList = new ListTag();
      for (DialogTextData dialogText : this.dialogTexts) {
        dialogTextsList.add(dialogText.write(new CompoundTag()));
      }
      compoundTag.put(DATA_TEXTS_TAG, dialogTextsList);
    }

    // Save buttons, if any.
    if (this.dialogButtons != null) {
      ListTag buttonsList = new ListTag();
      for (DialogButtonEntry button : this.dialogButtons) {
        buttonsList.add(button.write(new CompoundTag()));
      }
      compoundTag.put(DATA_BUTTONS_TAG, buttonsList);
    }

    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }

  @Override
  public String toString() {
    return "DialogData [id="
        + this.id
        + ", name="
        + this.name
        + ", label="
        + this.label
        + ", texts="
        + this.dialogTexts
        + ", buttons="
        + this.dialogButtons
        + "]";
  }
}
