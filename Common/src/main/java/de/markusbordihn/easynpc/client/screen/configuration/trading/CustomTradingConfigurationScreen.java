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

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.data.trading.TradingType;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingData;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import java.util.Collections;
import java.util.List;
import net.minecraft.network.chat.Component;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.entity.player.Inventory;

public class CustomTradingConfigurationScreen<T extends ConfigurationMenu>
    extends TradingConfigurationScreen<T> {

  private static TradingType formerTradingType = TradingType.BASIC;
  protected Checkbox noneTradingCheckbox;
  protected int numberOfTextLines = 1;
  private List<FormattedCharSequence> textComponents = Collections.emptyList();

  public CustomTradingConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  public static void setFormerTradingType(TradingType dialogType) {
    formerTradingType = dialogType;
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.customTradesButton.active = false;

    // Cache former dialog Type
    TradingData<?> tradingData = this.getEasyNPC().getEasyNPCTradingData();
    setFormerTradingType(tradingData.getTradingType());

    // Custom Trading Checkbox
    this.noneTradingCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 100,
                this.topPos + 170,
                "custom_trading_checkbox",
                tradingData.getTradingType() == TradingType.CUSTOM,
                checkbox -> {
                  if (checkbox.selected()) {
                    NetworkMessageHandlerManager.getServerHandler()
                        .changeTradingType(this.getNpcUUID(), TradingType.CUSTOM);
                  } else {
                    NetworkMessageHandlerManager.getServerHandler()
                        .changeTradingType(
                            this.getNpcUUID(),
                            formerTradingType != null && formerTradingType != TradingType.NONE
                                ? formerTradingType
                                : TradingType.NONE);
                  }
                }));

    // Pre-format text
    this.textComponents =
        this.font.split(
            Component.translatable(Constants.TEXT_CONFIG_PREFIX + "custom_trading_text"),
            this.imageWidth - 20);
    this.numberOfTextLines = this.textComponents.size();
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    if (!this.textComponents.isEmpty()) {
      for (int line = 0; line < this.numberOfTextLines; ++line) {
        FormattedCharSequence formattedCharSequence = this.textComponents.get(line);
        Text.drawString(
            poseStack,
            this.font,
            formattedCharSequence,
            leftPos + 15,
            topPos + 60 + (line * (font.lineHeight + 2)));
      }
    }
  }
}
