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

import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;

public class DialogData {

  // Limits
  public static final int MAX_DIALOG_LABEL_LENGTH = 32;

  // Dialog Data Tags
  public static final String DATA_BUTTONS_TAG = "Buttons";
  public static final String DATA_DIALOG_NAME = "Name";
  public static final String DATA_LABEL_TAG = "Label";
  public static final String DATA_TEXT_TAG = "Text";
  public static final String DATA_TEXTS_TAG = "Texts";
  public static final String DATA_TRANSLATE_TAG = "Translate";

  // Dialog Data
  private UUID id;
  private String label = "";
  private String name;
  private boolean translate;

  private Set<DialogTextData> dialogTexts = new LinkedHashSet<>();
  private Set<DialogButtonData> buttons = new LinkedHashSet<>();

  public DialogData(CompoundTag compoundTag) {
    this.load(compoundTag);
  }

  public DialogData(String name) {
    this(null, name, "Dialog text", false, null);
  }

  public DialogData(String name, String text, boolean translate) {
    this("default", name, text, translate, null);
  }

  public DialogData(String label, String name, String text, boolean translate) {
    this(label, name, text, translate, null);
  }

  public DialogData(
      String label, String name, String text, boolean translate, Set<DialogButtonData> buttons) {
    this.label = DialogUtils.generateButtonLabel(label != null && !label.isEmpty() ? label : name);
    this.name = name != null ? name.trim() : this.label;
    this.id = UUID.nameUUIDFromBytes(this.label.getBytes());
    this.buttons = buttons != null ? buttons : new LinkedHashSet<>();
    this.dialogTexts.add(new DialogTextData(text, translate));
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
    return this.dialogTexts.stream().findFirst().get().getText();
  }

  public void setText(String text) {
    this.dialogTexts.clear();
    this.dialogTexts.add(new DialogTextData(text));
  }

  public void setText(String text, UUID dialogTextId) {
    for (DialogTextData dialogText : this.dialogTexts) {
      if (dialogText.getId().equals(dialogTextId)) {
        dialogText.setText(text);
        return;
      }
    }
  }

  public String getDialogText() {
    if (this.dialogTexts == null || this.dialogTexts.isEmpty()) {
      return "";
    }

    // Return first dialog text or random dialog text.
    DialogTextData dialogText;
    if (this.dialogTexts.size() == 1) {
      dialogText = this.dialogTexts.stream().findFirst().orElse(null);
    } else {
      // Return random dialog text
      dialogText =
          this.dialogTexts.stream()
              .skip(ThreadLocalRandom.current().nextInt(this.dialogTexts.size()))
              .findFirst()
              .orElse(null);
    }
    return dialogText != null ? dialogText.getDialogText() : "";
  }

  public String getText(int maxLength) {
    return this.dialogTexts.stream().findFirst().get().getText(maxLength);
  }

  public String getDialogText(LivingEntity entity, Player player) {
    return DialogUtils.parseDialogText(getDialogText(), entity, player);
  }

  public DialogTextData getDialogText(UUID dialogTextId) {
    for (DialogTextData dialogText : this.dialogTexts) {
      if (dialogText.getId().equals(dialogTextId)) {
        return dialogText;
      }
    }
    return null;
  }

  public Set<DialogTextData> getDialogTexts() {
    return this.dialogTexts;
  }

  public void setDialogTexts(Set<DialogTextData> dialogTexts) {
    this.dialogTexts = dialogTexts;
  }

  public int getRandomDialogIndex() {
    return ThreadLocalRandom.current().nextInt(this.dialogTexts.size());
  }

  public boolean getTranslate() {
    return this.translate;
  }

  public void setTranslate(boolean translate) {
    this.translate = translate;
  }

  public Set<DialogButtonData> getButtons() {
    return this.buttons;
  }

  public void setButtons(Set<DialogButtonData> buttons) {
    this.buttons = buttons;
  }

  public DialogButtonData getButton(UUID dialogButtonId) {
    for (DialogButtonData button : this.buttons) {
      if (button.getId().equals(dialogButtonId)) {
        return button;
      }
    }
    return null;
  }

  public DialogButtonData getButton(String label) {
    for (DialogButtonData button : this.buttons) {
      if (button.getLabel().equals(label)) {
        return button;
      }
    }
    return null;
  }

  public void setButton(DialogButtonData dialogButtonData) {
    this.setButton(dialogButtonData.getId(), dialogButtonData);
  }

  public void setButton(UUID dialogButtonId, DialogButtonData dialogButtonData) {
    if (dialogButtonId != null) {
      for (DialogButtonData button : this.buttons) {
        if (button.getId().equals(dialogButtonId)) {
          this.buttons.remove(button);
          this.buttons.add(dialogButtonData);
          return;
        }
      }
    }
    this.buttons.add(dialogButtonData);
  }

  public boolean hasButton(String label) {
    for (DialogButtonData button : this.buttons) {
      if (button.getLabel().equals(label)) {
        return true;
      }
    }
    return false;
  }

  public boolean hasButton(UUID dialogButtonId) {
    for (DialogButtonData button : this.buttons) {
      if (button.getId().equals(dialogButtonId)) {
        return true;
      }
    }
    return false;
  }

  public boolean removeButton(UUID dialogButtonId) {
    for (DialogButtonData button : this.buttons) {
      if (button.getId().equals(dialogButtonId)) {
        this.buttons.remove(button);
        return true;
      }
    }
    return false;
  }

  public int getNumberOfButtons() {
    return this.buttons.size();
  }

  public void load(CompoundTag compoundTag) {
    this.name = compoundTag.getString(DATA_DIALOG_NAME);

    // Handle label and id creation
    if (compoundTag.contains(DATA_LABEL_TAG)) {
      this.setLabel(compoundTag.getString(DATA_LABEL_TAG));
    } else {
      this.setLabel(this.name);
    }

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
      this.dialogTexts.add(
          new DialogTextData(
              compoundTag.getString(DATA_TEXT_TAG),
              compoundTag.contains(DATA_TRANSLATE_TAG)
                  && compoundTag.getBoolean(DATA_TRANSLATE_TAG)));
    }

    // Load buttons, if available.
    if (compoundTag.contains(DATA_BUTTONS_TAG)) {
      this.buttons.clear();
      ListTag buttonsList = compoundTag.getList(DATA_BUTTONS_TAG, 10);
      if (!buttonsList.isEmpty()) {
        for (int i = 0; i < buttonsList.size(); i++) {
          this.buttons.add(new DialogButtonData(buttonsList.getCompound(i)));
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

    // Only save translate if it is true.
    if (this.translate) {
      compoundTag.putBoolean(DATA_TRANSLATE_TAG, this.translate);
    }

    // Save dialog text
    if (this.dialogTexts != null && !this.dialogTexts.isEmpty()) {
      ListTag dialogTextsList = new ListTag();
      for (DialogTextData dialogText : this.dialogTexts) {
        dialogTextsList.add(dialogText.save(new CompoundTag()));
      }
      compoundTag.put(DATA_TEXTS_TAG, dialogTextsList);
    }

    // Save buttons, if any.
    if (this.buttons != null) {
      ListTag buttonsList = new ListTag();
      for (DialogButtonData button : this.buttons) {
        buttonsList.add(button.save(new CompoundTag()));
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
        + ", translate="
        + this.translate
        + ", texts="
        + this.dialogTexts
        + ", buttons="
        + this.buttons
        + "]";
  }
}
