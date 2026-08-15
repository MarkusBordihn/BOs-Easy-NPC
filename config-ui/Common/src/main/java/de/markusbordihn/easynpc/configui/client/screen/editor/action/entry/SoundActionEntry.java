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
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.components.HelpIcon;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.SoundActionData;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.ValueUtils;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.stream.Collectors;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.sounds.SoundSource;

public class SoundActionEntry extends ActionEntryWidget {

  private static final int FIELD_WIDTH = 275;
  private static final int HELP_ICON_HEIGHT = 12;
  private static final int LABEL_HEIGHT = 8;
  private static final int LABEL_LEFT = 2;
  private static final int NUMBER_FIELD_WIDTH = 60;
  private static final int SECOND_COLUMN_LEFT = 140;
  private static final int SOUND_FIELD_TOP = 13;
  private static final int SOUND_ID_MAX_LENGTH = 128;
  private static final int SOUND_LABEL_TOP = 2;
  private static final int SOURCE_FIELD_TOP = 44;
  private static final int SOURCE_LABEL_TOP = 33;
  private static final int VALUE_FIELD_TOP = 75;
  private static final int VALUE_LABEL_TOP = 64;

  private TextField soundIdField;
  private SpinButton<SoundSource> soundSourceButton;
  private TextField volumeField;
  private TextField pitchField;

  public SoundActionEntry(
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
    SoundActionData soundActionData = this.currentSoundActionData();

    this.soundIdField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft, editorTop + SOUND_FIELD_TOP, FIELD_WIDTH, 16));
    this.soundIdField.setMaxLength(SOUND_ID_MAX_LENGTH);
    this.soundIdField.setValue(soundActionData.soundId());
    this.screen.addActionEntryWidget(
        new HelpIcon(
            this.helpIconLeft(editorLeft, "action.sound"),
            centeredTop(editorTop + SOUND_LABEL_TOP, HELP_ICON_HEIGHT),
            "action.sound.tooltip"));

    this.soundSourceButton =
        this.screen.addActionEntryWidget(
            new SpinButton<>(
                editorLeft,
                editorTop + SOURCE_FIELD_TOP,
                FIELD_WIDTH,
                16,
                Arrays.stream(SoundSource.values())
                    .collect(Collectors.toCollection(LinkedHashSet::new)),
                soundActionData.soundSource(),
                button -> {}));
    this.soundSourceButton.setLabelProvider(
        soundSource ->
            TextComponent.getTranslatedTextRaw("soundCategory." + soundSource.getName()));

    this.volumeField =
        this.screen.addActionEntryWidget(
            new TextField(
                this.font, editorLeft, editorTop + VALUE_FIELD_TOP, NUMBER_FIELD_WIDTH, 16));
    this.volumeField.setFilter(ValueUtils::isFloatValue);
    this.volumeField.setValue(String.valueOf(soundActionData.volume()));

    this.pitchField =
        this.screen.addActionEntryWidget(
            new TextField(
                this.font,
                editorLeft + SECOND_COLUMN_LEFT,
                editorTop + VALUE_FIELD_TOP,
                NUMBER_FIELD_WIDTH,
                16));
    this.pitchField.setFilter(ValueUtils::isFloatValue);
    this.pitchField.setValue(String.valueOf(soundActionData.pitch()));
  }

  private int helpIconLeft(int editorLeft, String labelTranslationKey) {
    return editorLeft
        + LABEL_LEFT
        + this.font.width(TextComponent.getTranslatedConfigText(labelTranslationKey))
        + 4;
  }

  @Override
  public void render(GuiGraphicsExtractor guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.sound",
        editorLeft + LABEL_LEFT,
        editorTop + SOUND_LABEL_TOP,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.sound.source",
        editorLeft + LABEL_LEFT,
        editorTop + SOURCE_LABEL_TOP,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.sound.volume",
        editorLeft + LABEL_LEFT,
        editorTop + VALUE_LABEL_TOP,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.sound.pitch",
        editorLeft + SECOND_COLUMN_LEFT + LABEL_LEFT,
        editorTop + VALUE_LABEL_TOP,
        Constants.FONT_COLOR_DEFAULT);

    if (!this.isValid()) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "action.sound.missing",
          editorLeft + SECOND_COLUMN_LEFT,
          editorTop + SOUND_LABEL_TOP,
          Constants.FONT_COLOR_RED);
    }
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    return new ActionDataEntry(ActionDataType.SOUND)
        .withSoundActionData(this.selectedSoundActionData());
  }

  @Override
  public boolean hasChanged() {
    return !this.hasActionData(ActionDataType.SOUND)
        || !this.selectedSoundActionData().equals(this.currentSoundActionData());
  }

  @Override
  public boolean isValid() {
    return this.soundIdField != null && this.selectedSoundActionData().hasSoundId();
  }

  private SoundActionData currentSoundActionData() {
    if (!this.hasActionData(ActionDataType.SOUND)
        || this.actionDataEntry.soundActionData() == null) {
      return SoundActionData.DEFAULT;
    }

    return this.actionDataEntry.soundActionData();
  }

  private SoundActionData selectedSoundActionData() {
    if (this.soundIdField == null) {
      return SoundActionData.DEFAULT;
    }

    return new SoundActionData(
        this.soundIdField.getValue(),
        this.soundSourceButton.get(),
        this.floatValue(this.volumeField, SoundActionData.DEFAULT_VOLUME),
        this.floatValue(this.pitchField, SoundActionData.DEFAULT_PITCH));
  }

  private float floatValue(TextField textField, float defaultValue) {
    return textField.getValue().isEmpty() ? defaultValue : Float.parseFloat(textField.getValue());
  }
}
