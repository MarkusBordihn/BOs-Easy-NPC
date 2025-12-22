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

import de.markusbordihn.easynpc.client.screen.components.ReloadButton;
import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.client.screen.editor.condition.ConditionDataEntryEditorContainerScreen;
import de.markusbordihn.easynpc.configui.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.configui.network.message.server.ResetExecutionLimitMessage;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.execution.ExecutionInterval;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.stream.Collectors;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;

public class ExecutionLimitConditionEntry extends ConditionEntryWidget {
  private TextField maxExecutionsTextField;
  private SpinButton<ExecutionInterval> executionIntervalButton;

  public ExecutionLimitConditionEntry(
      ConditionDataEntry conditionDataEntry,
      ConditionDataSet conditionDataSet,
      ConditionDataEntryEditorContainerScreen<?> screen) {
    super(conditionDataEntry, conditionDataSet, screen);
  }

  @Override
  public void init(int editorLeft, int editorTop) {
    boolean hasConditionData = hasConditionData(ConditionType.EXECUTION_LIMIT);
    this.maxExecutionsTextField =
        this.screen.addConditionEntryWidget(
            new TextField(
                this.font,
                editorLeft,
                editorTop + 10,
                35,
                hasConditionData ? String.valueOf(this.conditionDataEntry.value()) : "1",
                4));
    this.executionIntervalButton =
        this.screen.addConditionEntryWidget(
            new SpinButton<>(
                maxExecutionsTextField.getX() + maxExecutionsTextField.getWidth() + 5,
                maxExecutionsTextField.getY(),
                150,
                16,
                Arrays.stream(ExecutionInterval.values())
                    .sorted()
                    .collect(Collectors.toCollection(LinkedHashSet::new)),
                hasConditionData
                    ? ExecutionInterval.get(this.conditionDataEntry.text())
                    : ExecutionInterval.PER_DAY,
                button -> {}));

    ReloadButton resetCurrentPlayerButton =
        this.screen.addConditionEntryWidget(
            new ReloadButton(
                editorLeft + 5,
                this.executionIntervalButton.getY() + this.executionIntervalButton.getHeight() + 65,
                300,
                16,
                "condition.execution_limit.reset_current_player",
                onPress ->
                    NetworkHandlerManager.sendMessageToServer(
                        new ResetExecutionLimitMessage(this.screen.getDialogUUID(), false))));

    ReloadButton resetAllPlayersButton =
        this.screen.addConditionEntryWidget(
            new ReloadButton(
                resetCurrentPlayerButton.getX(),
                resetCurrentPlayerButton.getY() + resetCurrentPlayerButton.getHeight() + 5,
                300,
                16,
                "condition.execution_limit.reset_all_players",
                onPress ->
                    NetworkHandlerManager.sendMessageToServer(
                        new ResetExecutionLimitMessage(this.screen.getDialogUUID(), true))));

    resetAllPlayersButton.active =
        Minecraft.getInstance().player != null && Minecraft.getInstance().player.hasPermissions(2);
  }

  @Override
  public void render(GuiGraphics guiGraphics, int editorLeft, int editorTop) {
    if (this.maxExecutionsTextField != null) {
      Text.drawConfigString(
          guiGraphics,
          this.font,
          "condition.execution_limit.max_executions",
          editorLeft,
          this.maxExecutionsTextField.getY() - 12,
          Constants.FONT_COLOR_BLACK);
    }
  }

  @Override
  public ConditionDataEntry getConditionDataEntry() {
    int value = 1;
    if (this.maxExecutionsTextField != null) {
      try {
        value = Integer.parseInt(this.maxExecutionsTextField.getValue());
      } catch (NumberFormatException ignored) {
        // Ignore invalid number format and use default value.
      }
    }
    return new ConditionDataEntry(
        ConditionType.EXECUTION_LIMIT,
        ConditionOperationType.NONE,
        "",
        value,
        this.executionIntervalButton != null
            ? this.executionIntervalButton.get().name()
            : ExecutionInterval.PER_DAY.name());
  }
}
