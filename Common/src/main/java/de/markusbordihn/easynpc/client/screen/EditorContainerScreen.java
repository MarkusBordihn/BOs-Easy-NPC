/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.client.screen;

import com.mojang.blaze3d.vertex.PoseStack;
import de.markusbordihn.easynpc.menu.EasyNPCMenu;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class EditorContainerScreen<T extends EasyNPCMenu> extends ContainerScreen<T> {

  protected final ClientLevel clientLevel;
  protected final LocalPlayer localPlayer;

  public EditorContainerScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);

    // General environment Data
    this.localPlayer = this.minecraftInstance.player;
    this.clientLevel = this.minecraftInstance.level;
  }

  @Override
  public void init() {
    super.init();

    // Allow repeated key events
    if (this.minecraft != null) {
      this.minecraft.keyboardHandler.setSendRepeatsToGui(true);
    }
  }

  @Override
  public void removed() {
    if (this.minecraft != null) {
      this.minecraft.keyboardHandler.setSendRepeatsToGui(false);
    }
    super.removed();
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);
  }

  @Override
  protected void renderLabels(PoseStack poseStack, int x, int y) {
    // No labels
  }
}
