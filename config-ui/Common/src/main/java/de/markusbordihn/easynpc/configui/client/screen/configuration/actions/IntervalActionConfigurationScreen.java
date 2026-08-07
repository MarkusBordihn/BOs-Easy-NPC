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

package de.markusbordihn.easynpc.configui.client.screen.configuration.actions;

import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import java.util.List;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class IntervalActionConfigurationScreen<T extends ConfigurationMenu>
    extends ActionConfigurationScreen<T> {

  private static final List<ActionEventType> INTERVAL_ACTION_EVENT_TYPES =
      List.of(
          ActionEventType.ON_INTERVAL_INSTANT,
          ActionEventType.ON_INTERVAL_SHORT,
          ActionEventType.ON_INTERVAL_NORMAL,
          ActionEventType.ON_INTERVAL_LONG,
          ActionEventType.ON_INTERVAL_VERY_LONG);
  private static final int ACTION_BUTTON_SPACING = 40;

  public IntervalActionConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    this.intervalActionButton.active = false;

    int actionButtonTop = this.contentTopPos + 10;
    for (ActionEventType actionEventType : INTERVAL_ACTION_EVENT_TYPES) {
      this.addRenderableWidget(
          this.getActionDataButton(
              this.contentLeftPos,
              actionButtonTop,
              actionEventType,
              ConfigurationType.INTERVAL_ACTION));
      actionButtonTop += ACTION_BUTTON_SPACING;
    }
  }
}
