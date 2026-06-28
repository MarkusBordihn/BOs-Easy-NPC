/*
 * Copyright 2025 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.client.screen.editor.condition.entry;

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.ConditionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.stream.Collectors;
import net.minecraft.client.gui.GuiGraphicsExtractor;

public class TimeOfDayConditionEntry extends ConditionEntryWidget {

  private static final int MAX_DAY_TIME = 24000;

  private SpinButton<ConditionOperationType> operationTypeButton;
  private SpinButton<TimeOfDayPreset> presetButton;
  private TextField valueTextField;

  public TimeOfDayConditionEntry(
      ConditionDataEntry conditionDataEntry,
      ConditionDataSet conditionDataSet,
      ConditionDataEntryEditorContainerScreen<?> screen) {
    super(conditionDataEntry, conditionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasData = hasConditionData(ConditionType.TIME_OF_DAY);
    int currentValue = hasData ? this.conditionDataEntry.value() : TimeOfDayPreset.DAY.ticks();

    this.operationTypeButton =
        this.screen.addConditionEntryWidget(
            new SpinButton<>(
                editorLeft + 110,
                editorTop,
                180,
                16,
                Arrays.stream(ConditionOperationType.values())
                    .filter(type -> type != ConditionOperationType.NONE)
                    .sorted()
                    .collect(Collectors.toCollection(LinkedHashSet::new)),
                hasData
                    ? this.conditionDataEntry.operationType()
                    : ConditionOperationType.GREATER_THAN_OR_EQUALS,
                button -> {}));

    this.valueTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font, editorLeft + 110, editorTop + 50, 180, String.valueOf(currentValue), 5));

    this.presetButton =
        this.screen.addConditionEntryWidget(
            new SpinButton<>(
                editorLeft + 110,
                editorTop + 25,
                180,
                16,
                Arrays.stream(TimeOfDayPreset.values())
                    .collect(Collectors.toCollection(LinkedHashSet::new)),
                TimeOfDayPreset.fromTicks(currentValue),
                button -> {
                  TimeOfDayPreset preset = button.get();
                  if (preset != null && preset != TimeOfDayPreset.CUSTOM) {
                    this.valueTextField.setValue(String.valueOf(preset.ticks()));
                  }
                }));
  }

  @Override
  public void render(GuiGraphicsExtractor guiGraphics, int editorLeft, int editorTop) {
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.time_of_day.operation",
        editorLeft,
        editorTop + 4,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.time_of_day.preset",
        editorLeft,
        editorTop + 29,
        Constants.FONT_COLOR_BLACK);
    Text.drawConfigString(
        guiGraphics,
        this.font,
        "condition.time_of_day.value",
        editorLeft,
        editorTop + 54,
        Constants.FONT_COLOR_BLACK);
  }

  @Override
  public ConditionDataEntry getConditionDataEntry() {
    int value = TimeOfDayPreset.DAY.ticks();
    if (this.valueTextField != null) {
      try {
        value =
            Math.max(0, Math.min(MAX_DAY_TIME, Integer.parseInt(this.valueTextField.getValue())));
      } catch (NumberFormatException ignored) {
        value = TimeOfDayPreset.DAY.ticks();
      }
    }
    return new ConditionDataEntry(
        ConditionType.TIME_OF_DAY,
        this.operationTypeButton != null
            ? this.operationTypeButton.get()
            : ConditionOperationType.GREATER_THAN_OR_EQUALS,
        "",
        value);
  }

  private enum TimeOfDayPreset {
    DAY(1000),
    NOON(6000),
    SUNSET(12000),
    NIGHT(13000),
    MIDNIGHT(18000),
    SUNRISE(23000),
    CUSTOM(-1);

    private final int ticks;

    TimeOfDayPreset(int ticks) {
      this.ticks = ticks;
    }

    static TimeOfDayPreset fromTicks(int ticks) {
      for (TimeOfDayPreset preset : values()) {
        if (preset != CUSTOM && preset.ticks == ticks) {
          return preset;
        }
      }
      return CUSTOM;
    }

    int ticks() {
      return this.ticks;
    }

    @Override
    public String toString() {
      if (this == CUSTOM) {
        return "Custom";
      }
      String name = name().charAt(0) + name().substring(1).toLowerCase();
      return name + " (" + this.ticks + ")";
    }
  }
}
