/**
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

import net.minecraft.client.gui.components.Button;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;

@OnlyIn(Dist.CLIENT)
public class TradingConfigurationScreen<T extends ConfigurationMenu>
    extends ConfigurationScreen<T> {

  // Buttons
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
    this.noneTradesButton = this.addRenderableWidget(
        menuButton(this.buttonLeftPos, this.buttonTopPos, 64, "disable_trading", onPress -> {
          NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.NONE_TRADING);
        }));
    this.basicTradesButton =
        this.addRenderableWidget(menuButton(this.buttonLeftPos + this.noneTradesButton.getWidth(),
            this.buttonTopPos, 64, "basic_trading", onPress -> {
              NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.BASIC_TRADING);
            }));
    this.advancedTradesButton = this.addRenderableWidget(menuButton(
        this.buttonLeftPos + this.noneTradesButton.getWidth() + this.basicTradesButton.getWidth(),
        this.buttonTopPos, 70, "advanced_trading", onPress -> {
          NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.ADVANCED_TRADING);
        }));
    this.customTradesButton = this.addRenderableWidget(menuButton(
        this.buttonLeftPos + this.noneTradesButton.getWidth() + this.basicTradesButton.getWidth()
            + this.advancedTradesButton.getWidth(),
        this.buttonTopPos, 100, "custom_trading", onPress -> {
          NetworkMessageHandler.openConfiguration(uuid, ConfigurationType.CUSTOM_TRADING);
        }));

    // Default button stats
    this.noneTradesButton.active = this.hasPermissions(COMMON.noneTradingConfigurationEnabled.get(),
        COMMON.noneTradingConfigurationAllowInCreative.get(),
        COMMON.noneTradingConfigurationPermissionLevel.get());
    this.basicTradesButton.active =
        this.hasPermissions(COMMON.basicTradingConfigurationEnabled.get(),
            COMMON.basicTradingConfigurationAllowInCreative.get(),
            COMMON.basicTradingConfigurationPermissionLevel.get());
    this.advancedTradesButton.active =
        this.hasPermissions(COMMON.advancedTradingConfigurationEnabled.get(),
            COMMON.advancedTradingConfigurationAllowInCreative.get(),
            COMMON.advancedTradingConfigurationPermissionLevel.get());
    this.customTradesButton.active =
        this.hasPermissions(COMMON.customTradingConfigurationEnabled.get(),
            COMMON.customTradingConfigurationAllowInCreative.get(),
            COMMON.customTradingConfigurationPermissionLevel.get());
  }

}
