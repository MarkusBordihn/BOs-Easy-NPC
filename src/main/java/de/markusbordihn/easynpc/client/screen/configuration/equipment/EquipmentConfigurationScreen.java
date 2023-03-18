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

package de.markusbordihn.easynpc.client.screen.configuration.equipment;

import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.menu.configuration.equipment.EquipmentConfigurationMenu;

@OnlyIn(Dist.CLIENT)
public class EquipmentConfigurationScreen extends ConfigurationScreen<EquipmentConfigurationMenu> {

  public EquipmentConfigurationScreen(EquipmentConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Basic Position
    this.inventoryLabelX = 8;
    this.inventoryLabelY = this.imageHeight - 92;
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);

    // Avatar
    ScreenHelper.renderScaledEntityAvatar(this.contentLeftPos + 138, this.contentTopPos + 82, 35,
        this.contentLeftPos + 140 - this.xMouse, this.contentTopPos + 30 - this.yMouse,
        this.entity);
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(poseStack, partialTicks, mouseX, mouseY);

    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_INVENTORY);

    // Armors Slots Left
    switch (this.skinModel) {
      case HUMANOID:
      case HUMANOID_SLIM:
      case SKELETON:
        blit(poseStack, this.contentLeftPos + 90, this.contentTopPos, 7, 7, 18, 72);
        break;
      default:
        break;
    }

    // Main Hand Slot Left
    blit(poseStack, this.contentLeftPos + 90, this.contentTopPos + 75, 7, 7, 18, 18);

    // Off Hand Slot Right
    switch (this.skinModel) {
      case HUMANOID:
      case HUMANOID_SLIM:
      case SKELETON:
        blit(poseStack, this.contentLeftPos + 170, this.contentTopPos + 75, 7, 7, 18, 18);
        break;
      default:
        break;
    }

    // Player Inventory Slots
    blit(poseStack, this.contentLeftPos + 58, this.contentTopPos + 105, 7, 83, 162, 54);

    // Player Hotbar Slots
    blit(poseStack, this.contentLeftPos + 58, this.contentTopPos + 165, 7, 141, 162, 18);

    // Entity
    fill(poseStack, this.contentLeftPos + 109, this.contentTopPos - 8, this.contentLeftPos + 169,
        this.contentTopPos + 102, 0xff000000);
    fill(poseStack, this.contentLeftPos + 110, this.contentTopPos - 7, this.contentLeftPos + 168,
        this.contentTopPos + 101, 0xffaaaaaa);
  }

}
