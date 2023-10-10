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

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.menu.configuration.trading.BasicTradingConfigurationMenu;

@OnlyIn(Dist.CLIENT)
public class BasicTradingConfigurationScreen
    extends TradingConfigurationScreen<BasicTradingConfigurationMenu> {

  public BasicTradingConfigurationScreen(BasicTradingConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.basicTradesButton.active = false;
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_INVENTORY);

    // Render Trading Slots
    for (int tradingOffer =
        0; tradingOffer < BasicTradingConfigurationMenu.TRADING_OFFERS; tradingOffer++) {
      int slotPositionY = this.topPos + BasicTradingConfigurationMenu.TRADING_START_POSITION_Y
          + (tradingOffer * (BasicTradingConfigurationMenu.TRADING_SLOT_SIZE + 1));

      // Item A Slot
      int itemASlotLeftPosition =
          this.leftPos + BasicTradingConfigurationMenu.TRADING_START_POSITION_X - 1;
      int itemASlotTopPosition = slotPositionY;
      RenderSystem.setShaderTexture(0, Constants.TEXTURE_INVENTORY);
      this.blit(poseStack, itemASlotLeftPosition, itemASlotTopPosition, 7, 7, 18, 18);

      // "+" Label
      font.draw(poseStack, "+",
          itemASlotLeftPosition + BasicTradingConfigurationMenu.TRADING_SLOT_SIZE + 6,
          itemASlotTopPosition + 5, 0x404040);

      // Item B Slot
      int itemBSlotLeftPosition =
          this.leftPos + BasicTradingConfigurationMenu.TRADING_START_POSITION_X - 1
              + BasicTradingConfigurationMenu.TRADING_SLOT_SIZE
              + BasicTradingConfigurationMenu.TRADING_SLOT_SIZE;
      int itemBSlotTopPosition = slotPositionY;
      RenderSystem.setShaderTexture(0, Constants.TEXTURE_INVENTORY);
      this.blit(poseStack, itemBSlotLeftPosition, itemBSlotTopPosition, 7, 7, 18, 18);

      // "=" Label
      font.draw(poseStack, "=",
          itemBSlotLeftPosition + BasicTradingConfigurationMenu.TRADING_SLOT_SIZE + 12,
          itemBSlotTopPosition + 5, 0x404040);

      // Result Slot
      RenderSystem.setShaderTexture(0, Constants.TEXTURE_INVENTORY);
      this.blit(poseStack,
          this.leftPos + BasicTradingConfigurationMenu.TRADING_START_POSITION_X - 1
              + ((BasicTradingConfigurationMenu.TRADING_SLOT_SIZE
                  + BasicTradingConfigurationMenu.TRADING_SLOT_SIZE + 5) * 2),
          slotPositionY, 7, 7, 18, 18);
    }

    // Player Inventory Slots
    this.blit(poseStack, this.contentLeftPos + 72, this.contentTopPos + 111, 7, 83, 162, 54);

    // Player Hotbar Slots
    this.blit(poseStack, this.contentLeftPos + 72, this.contentTopPos + 171, 7, 141, 162, 18);
  }

}
