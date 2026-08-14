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

package de.markusbordihn.easynpc.configui.client.screen.editor.action.entry;

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.components.HelpIcon;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.MessageActionData;
import de.markusbordihn.easynpc.data.action.MessageRecipientScope;
import de.markusbordihn.easynpc.entity.easynpc.handlers.action.executor.MessageActionExecutor;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.stream.Collectors;
import net.minecraft.client.gui.GuiGraphics;

public class MessageActionEntry extends ActionEntryWidget {

  private static final int FIELD_WIDTH = 275;
  private static final int HELP_ICON_HEIGHT = 12;
  private static final int LABEL_HEIGHT = 8;
  private static final int LABEL_LEFT = 2;
  private static final int MESSAGE_FIELD_TOP = 13;
  private static final int MESSAGE_LABEL_TOP = 2;
  private static final int RECIPIENT_LABEL_TOP = 67;
  private static final int RECIPIENT_ROW_TOP = 64;
  private static final int SECOND_COLUMN_LEFT = 90;
  private static final int SENDER_FIELD_TOP = 44;
  private static final int SENDER_LABEL_TOP = 33;
  private static final int TARGET_FIRST_ROW_TOP = 97;
  private static final int TARGET_LABEL_TOP = 86;
  private static final int TARGET_SECOND_ROW_TOP = 115;
  private static final int TEXT_NAVIGATION_HEIGHT = 10;
  private static final int TEXT_NAVIGATION_WIDTH = 113;

  private final List<String> messageTexts = new ArrayList<>();
  private TextField messageTextField;
  private TextField senderNameTextField;
  private Checkbox nearbyChatCheckbox;
  private Checkbox systemMessageCheckbox;
  private Checkbox speechBubbleCheckbox;
  private SpinButton<MessageRecipientScope> recipientScopeButton;
  private TextButton previousTextButton;
  private TextButton textIndexButton;
  private TextButton nextTextButton;
  private TextButton addTextButton;
  private TextButton removeTextButton;
  private int messageTextIndex;

  public MessageActionEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen) {
    super(actionDataEntry, actionDataSet, screen);
  }

  private static int centeredTop(int labelTop, int widgetHeight) {
    return labelTop + LABEL_HEIGHT / 2 - widgetHeight / 2;
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    MessageActionData messageActionData = this.currentMessageActionData();
    this.messageTexts.clear();
    this.messageTexts.addAll(messageActionData.texts());
    if (this.messageTexts.isEmpty()) {
      this.messageTexts.add("");
    }
    this.messageTextIndex = 0;

    this.messageTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft, editorTop + MESSAGE_FIELD_TOP, FIELD_WIDTH, 16));
    this.messageTextField.setMaxLength(MessageActionExecutor.MAX_MESSAGE_LENGTH);
    this.messageTextField.setValue(this.messageTexts.get(0));

    int navigationLeft = editorLeft + FIELD_WIDTH - TEXT_NAVIGATION_WIDTH;
    int navigationTop = centeredTop(editorTop + MESSAGE_LABEL_TOP, TEXT_NAVIGATION_HEIGHT);
    this.previousTextButton =
        this.screen.addActionEntryWidget(
            new TextButton(
                navigationLeft,
                navigationTop,
                16,
                TEXT_NAVIGATION_HEIGHT,
                "<",
                button -> this.previousText()));
    this.textIndexButton =
        this.screen.addActionEntryWidget(
            new TextButton(
                navigationLeft + 16, navigationTop, 45, TEXT_NAVIGATION_HEIGHT, "", button -> {}));
    this.textIndexButton.active = false;
    this.nextTextButton =
        this.screen.addActionEntryWidget(
            new TextButton(
                navigationLeft + 61,
                navigationTop,
                16,
                TEXT_NAVIGATION_HEIGHT,
                ">",
                button -> this.nextText()));
    this.addTextButton =
        this.screen.addActionEntryWidget(
            new TextButton(
                navigationLeft + 81,
                navigationTop,
                16,
                TEXT_NAVIGATION_HEIGHT,
                "+",
                button -> this.addText()));
    this.removeTextButton =
        this.screen.addActionEntryWidget(
            new TextButton(
                navigationLeft + 97,
                navigationTop,
                16,
                TEXT_NAVIGATION_HEIGHT,
                "-",
                button -> this.removeText()));
    this.screen.addActionEntryWidget(
        new HelpIcon(
            this.helpIconLeft(editorLeft, "action.message"),
            centeredTop(editorTop + MESSAGE_LABEL_TOP, HELP_ICON_HEIGHT),
            "action.message.texts.tooltip"));
    this.updateTextNavigation();

    this.senderNameTextField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft, editorTop + SENDER_FIELD_TOP, FIELD_WIDTH, 16));
    this.senderNameTextField.setMaxLength(MessageActionData.MAX_SENDER_NAME_LENGTH);
    this.senderNameTextField.setValue(messageActionData.senderName());
    this.screen.addActionEntryWidget(
        new HelpIcon(
            this.helpIconLeft(editorLeft, "action.message.sender"),
            centeredTop(editorTop + SENDER_LABEL_TOP, HELP_ICON_HEIGHT),
            "action.message.sender.tooltip"));

    this.recipientScopeButton =
        this.screen.addActionEntryWidget(
            new SpinButton<>(
                editorLeft + SECOND_COLUMN_LEFT,
                editorTop + RECIPIENT_ROW_TOP,
                FIELD_WIDTH - SECOND_COLUMN_LEFT,
                16,
                Arrays.stream(MessageRecipientScope.values())
                    .collect(Collectors.toCollection(LinkedHashSet::new)),
                messageActionData.recipientScope(),
                button -> {}));

    this.nearbyChatCheckbox =
        this.addTargetCheckbox(
            editorLeft,
            editorTop + TARGET_FIRST_ROW_TOP,
            "action.message.target.chat",
            messageActionData.showInNearbyChat());
    boolean hasServerPlayer = this.screen.currentEventRequiresServerPlayer();
    this.systemMessageCheckbox =
        this.addTargetCheckbox(
            editorLeft + SECOND_COLUMN_LEFT + 50,
            editorTop + TARGET_FIRST_ROW_TOP,
            "action.message.target.system",
            hasServerPlayer && messageActionData.showAsSystemMessage());
    this.systemMessageCheckbox.active = hasServerPlayer;
    this.speechBubbleCheckbox =
        this.addTargetCheckbox(
            editorLeft,
            editorTop + TARGET_SECOND_ROW_TOP,
            "action.message.target.bubble",
            messageActionData.showAsSpeechBubble());
  }

  private int helpIconLeft(int editorLeft, String labelTranslationKey) {
    return editorLeft
        + LABEL_LEFT
        + this.font.width(TextComponent.getTranslatedConfigText(labelTranslationKey))
        + 4;
  }

  private Checkbox addTargetCheckbox(int left, int top, String translationKey, boolean selected) {
    return this.screen.addActionEntryWidget(
        new Checkbox(
            left, top, TextComponent.getTranslatedConfigText(translationKey), selected, true));
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.message",
        editorLeft + LABEL_LEFT,
        editorTop + MESSAGE_LABEL_TOP,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.message.sender",
        editorLeft + LABEL_LEFT,
        editorTop + SENDER_LABEL_TOP,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.message.recipient",
        editorLeft + LABEL_LEFT,
        editorTop + RECIPIENT_LABEL_TOP,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.message.target",
        editorLeft + LABEL_LEFT,
        editorTop + TARGET_LABEL_TOP,
        Constants.FONT_COLOR_DEFAULT);

    if (!this.hasAnyTarget()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "action.message.target.missing",
          editorLeft + SECOND_COLUMN_LEFT,
          editorTop + TARGET_LABEL_TOP,
          Constants.FONT_COLOR_RED);
    }
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    List<String> texts = this.collectMessageTexts();
    return new ActionDataEntry(ActionDataType.MESSAGE)
        .withMessageActionData(this.selectedMessageActionData().withTexts(texts));
  }

  @Override
  public boolean hasChanged() {
    List<String> texts = this.collectMessageTexts();
    return !texts.equals(this.currentMessageActionData().texts())
        || !this.selectedMessageActionData()
            .withTexts(texts)
            .equals(this.currentMessageActionData());
  }

  @Override
  public boolean isValid() {
    return !this.collectMessageTexts().isEmpty() && this.hasAnyTarget();
  }

  private void previousText() {
    if (this.messageTextIndex <= 0) {
      return;
    }

    this.storeCurrentText();
    this.messageTextIndex--;
    this.loadCurrentText();
  }

  private void nextText() {
    if (this.messageTextIndex >= this.messageTexts.size() - 1) {
      return;
    }

    this.storeCurrentText();
    this.messageTextIndex++;
    this.loadCurrentText();
  }

  private void addText() {
    if (this.messageTexts.size() >= MessageActionData.MAX_TEXTS) {
      return;
    }

    this.storeCurrentText();
    this.messageTexts.add("");
    this.messageTextIndex = this.messageTexts.size() - 1;
    this.loadCurrentText();
  }

  private void removeText() {
    if (this.messageTexts.size() <= 1) {
      return;
    }

    this.messageTexts.remove(this.messageTextIndex);
    this.messageTextIndex = Math.min(this.messageTextIndex, this.messageTexts.size() - 1);
    this.loadCurrentText();
  }

  private void storeCurrentText() {
    this.messageTexts.set(this.messageTextIndex, this.messageTextField.getValue());
  }

  private void loadCurrentText() {
    this.messageTextField.setValue(this.messageTexts.get(this.messageTextIndex));
    this.updateTextNavigation();
  }

  private void updateTextNavigation() {
    this.textIndexButton.setMessage(
        TextComponent.getText((this.messageTextIndex + 1) + " / " + this.messageTexts.size()));
    this.previousTextButton.active = this.messageTextIndex > 0;
    this.nextTextButton.active = this.messageTextIndex < this.messageTexts.size() - 1;
    this.addTextButton.active = this.messageTexts.size() < MessageActionData.MAX_TEXTS;
    this.removeTextButton.active = this.messageTexts.size() > 1;
  }

  private List<String> collectMessageTexts() {
    this.storeCurrentText();
    return this.messageTexts.stream().map(String::trim).filter(text -> !text.isEmpty()).toList();
  }

  private boolean hasAnyTarget() {
    return this.nearbyChatCheckbox != null
        && (this.nearbyChatCheckbox.selected()
            || this.systemMessageCheckbox.selected()
            || this.speechBubbleCheckbox.selected());
  }

  private MessageActionData currentMessageActionData() {
    if (!hasActionData(ActionDataType.MESSAGE)
        || this.actionDataEntry.messageActionData() == null) {
      return MessageActionData.DEFAULT;
    }

    return this.actionDataEntry.messageActionData();
  }

  private MessageActionData selectedMessageActionData() {
    if (this.nearbyChatCheckbox == null) {
      return MessageActionData.DEFAULT;
    }

    return new MessageActionData(
        this.nearbyChatCheckbox.selected(),
        this.systemMessageCheckbox.selected(),
        this.speechBubbleCheckbox.selected(),
        this.senderNameTextField.getValue(),
        this.recipientScopeButton.get());
  }
}
