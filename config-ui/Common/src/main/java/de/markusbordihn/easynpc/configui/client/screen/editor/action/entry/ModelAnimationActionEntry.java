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

import de.markusbordihn.easynpc.api.animation.ModelAnimationAPI;
import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.editor.action.ActionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ModelAnimationActionData;
import de.markusbordihn.easynpc.data.model.ModelAnimationPlayback;
import de.markusbordihn.easynpc.data.model.ModelAnimationPlaybackMode;
import de.markusbordihn.easynpc.data.model.ModelAnimationSwitchTiming;
import de.markusbordihn.easynpc.data.model.ModelAnimationTransition;
import de.markusbordihn.easynpc.network.components.TextComponent;
import de.markusbordihn.easynpc.utils.ValueUtils;
import java.util.EnumSet;
import java.util.LinkedHashSet;
import java.util.Locale;
import net.minecraft.client.gui.GuiGraphicsExtractor;

public class ModelAnimationActionEntry extends ActionEntryWidget {

  private final ActionDataType actionDataType;
  private TextField animationNameField;
  private TextField blendTicksField;
  private TextField repeatCountField;
  private TextField durationTicksField;
  private SpinButton<ModelAnimationPlaybackMode> playbackModeButton;
  private Checkbox afterCurrentCheckbox;

  public ModelAnimationActionEntry(
      ActionDataEntry actionDataEntry,
      ActionDataSet actionDataSet,
      ActionDataEntryEditorContainerScreen<?> screen,
      ActionDataType actionDataType) {
    super(actionDataEntry, actionDataSet, screen);
    this.actionDataType = actionDataType;
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    ModelAnimationActionData data =
        hasActionData(this.actionDataType)
            ? this.actionDataEntry.modelAnimationActionData()
            : ModelAnimationActionData.DEFAULT;

    if (this.actionDataType == ActionDataType.PLAY_ANIMATION) {
      LinkedHashSet<String> animations = new LinkedHashSet<>();
      ModelAnimationAPI.listAnimations(this.screen.getEasyNPC())
          .forEach(
              info ->
                  animations.add(
                      ModelAnimationAPI.standardAnimations().contains(info.name())
                          ? info.name()
                          : "named:" + info.name()));
      if (animations.isEmpty()) {
        animations.addAll(ModelAnimationAPI.standardAnimations());
      }

      this.animationNameField =
          this.screen.addActionEntryWidget(
              new TextField(this.font, editorLeft + 105, editorTop + 20, 170, 16));
      this.animationNameField.setMaxLength(ModelAnimationAPI.MAX_ANIMATION_NAME_LENGTH);
      this.animationNameField.setValue(data.animationName());
      String selected =
          animations.contains(data.animationName())
              ? data.animationName()
              : animations.iterator().next();
      this.screen.addActionEntryWidget(
          new SpinButton<>(
              editorLeft,
              editorTop + 20,
              100,
              16,
              animations,
              selected,
              button -> this.animationNameField.setValue(button.get())));

      this.playbackModeButton =
          this.screen.addActionEntryWidget(
              new SpinButton<>(
                  editorLeft,
                  editorTop + 45,
                  100,
                  16,
                  EnumSet.allOf(ModelAnimationPlaybackMode.class),
                  data.playback().mode(),
                  null));
      this.playbackModeButton.setLabelProvider(
          playbackMode ->
              TextComponent.getTranslatedConfigText(playbackMode.name().toLowerCase(Locale.ROOT)));

      this.repeatCountField =
          this.screen.addActionEntryWidget(
              new TextField(this.font, editorLeft + 105, editorTop + 95, 40, 16));
      this.repeatCountField.setFilter(ValueUtils::isNumericValue);
      this.repeatCountField.setValue(String.valueOf(data.playback().repeatCount()));

      this.durationTicksField =
          this.screen.addActionEntryWidget(
              new TextField(this.font, editorLeft + 105, editorTop + 120, 40, 16));
      this.durationTicksField.setFilter(ValueUtils::isFloatValue);
      this.durationTicksField.setValue(String.valueOf(data.playback().durationTicks()));
    }

    this.afterCurrentCheckbox =
        this.screen.addActionEntryWidget(
            new Checkbox(
                editorLeft + (this.actionDataType == ActionDataType.PLAY_ANIMATION ? 100 : 0),
                editorTop + 45,
                "after_current",
                data.transition().timing() == ModelAnimationSwitchTiming.AFTER_CURRENT));

    this.blendTicksField =
        this.screen.addActionEntryWidget(
            new TextField(this.font, editorLeft + 105, editorTop + 70, 80, 16));
    this.blendTicksField.setFilter(ValueUtils::isFloatValue);
    this.blendTicksField.setValue(String.valueOf(data.transition().blendDurationTicks()));
  }

  @Override
  public void render(GuiGraphicsExtractor guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        this.actionDataType == ActionDataType.PLAY_ANIMATION
            ? "action.animation"
            : "action.animation.transition",
        editorLeft + 2,
        editorTop + 5,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.animation.blend",
        editorLeft + 2,
        editorTop + 73,
        Constants.FONT_COLOR_DEFAULT);
    if (this.actionDataType != ActionDataType.PLAY_ANIMATION) {
      return;
    }

    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.animation.repeat_count",
        editorLeft + 2,
        editorTop + 98,
        Constants.FONT_COLOR_DEFAULT);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "action.animation.duration",
        editorLeft + 2,
        editorTop + 123,
        Constants.FONT_COLOR_DEFAULT);
  }

  @Override
  public boolean isValid() {
    return this.actionDataType != ActionDataType.PLAY_ANIMATION
        || (this.animationNameField != null
            && !ModelAnimationAPI.normalizeAnimationName(this.animationNameField.getValue())
                .isEmpty());
  }

  @Override
  public boolean hasChanged() {
    return !hasActionData(this.actionDataType)
        || !this.getActionDataEntry()
            .modelAnimationActionData()
            .equals(this.actionDataEntry.modelAnimationActionData());
  }

  @Override
  public ActionDataEntry getActionDataEntry() {
    float blendTicks =
        this.blendTicksField.getValue().isEmpty()
            ? ModelAnimationTransition.DEFAULT_BLEND_DURATION_TICKS
            : Float.parseFloat(this.blendTicksField.getValue());
    ModelAnimationActionData data =
        new ModelAnimationActionData(
            this.animationNameField != null
                ? ModelAnimationAPI.normalizeAnimationName(this.animationNameField.getValue())
                : "",
            this.getPlayback(),
            new ModelAnimationTransition(
                this.afterCurrentCheckbox.selected()
                    ? ModelAnimationSwitchTiming.AFTER_CURRENT
                    : ModelAnimationSwitchTiming.IMMEDIATE,
                blendTicks));
    return new ActionDataEntry(this.actionDataType).withModelAnimationActionData(data);
  }

  private ModelAnimationPlayback getPlayback() {
    if (this.playbackModeButton == null) {
      return ModelAnimationPlayback.DEFAULT;
    }

    int repeatCount =
        this.repeatCountField.getValue().isEmpty()
            ? ModelAnimationPlayback.DEFAULT_REPEAT_COUNT
            : Integer.parseInt(this.repeatCountField.getValue());
    float durationTicks =
        this.durationTicksField.getValue().isEmpty()
            ? ModelAnimationPlayback.UNLIMITED_DURATION_TICKS
            : Float.parseFloat(this.durationTicksField.getValue());
    return new ModelAnimationPlayback(this.playbackModeButton.get(), repeatCount, durationTicks);
  }
}
