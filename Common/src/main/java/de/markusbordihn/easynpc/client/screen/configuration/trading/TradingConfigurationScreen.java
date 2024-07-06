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

package de.markusbordihn.easynpc.client.screen.configuration.trading;

import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class TradingConfigurationScreen<T extends ConfigurationMenu>
    extends ConfigurationScreen<T> {

  protected Button noneTradesButton = null;
  protected Button basicTradesButton = null;
  protected Button advancedTradesButton = null;
  protected Button customTradesButton = null;

  public TradingConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Trade Types
    this.noneTradesButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                64,
                "disable_trading",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getNpcUUID(), ConfigurationType.NONE_TRADING)));
    this.basicTradesButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos + this.noneTradesButton.getWidth(),
                this.buttonTopPos,
                64,
                "basic",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getNpcUUID(), ConfigurationType.BASIC_TRADING)));
    this.advancedTradesButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos
                    + this.noneTradesButton.getWidth()
                    + this.basicTradesButton.getWidth(),
                this.buttonTopPos,
                70,
                "advanced",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getNpcUUID(), ConfigurationType.ADVANCED_TRADING)));
    this.customTradesButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos
                    + this.noneTradesButton.getWidth()
                    + this.basicTradesButton.getWidth()
                    + this.advancedTradesButton.getWidth(),
                this.buttonTopPos,
                100,
                "custom",
                onPress ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getNpcUUID(), ConfigurationType.CUSTOM_TRADING)));
  }
}
