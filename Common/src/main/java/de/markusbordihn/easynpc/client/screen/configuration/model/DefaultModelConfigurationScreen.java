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

package de.markusbordihn.easynpc.client.screen.configuration.model;

import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.data.render.RenderDataSet;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class DefaultModelConfigurationScreen<T extends ConfigurationMenu>
    extends ModelConfigurationScreen<T> {

  protected Checkbox defaultModelCheckbox;

  public DefaultModelConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.defaultModelButton.active = false;

    // Description text
    setDescriptionText("default_model.text");

    // Render data
    RenderDataSet renderData = this.getRenderDataSet();
    RenderType renderType = renderData.getRenderType();

    // Default Model Checkbox
    this.defaultModelCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 90,
                this.topPos + 170,
                "default_model.checkbox",
                renderType == RenderType.DEFAULT,
                checkbox ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .setRenderType(
                            this.getEasyNPCUUID(),
                            checkbox.selected()
                                ? RenderType.DEFAULT
                                : renderType != null && renderType != RenderType.DEFAULT
                                    ? renderType
                                    : RenderType.CUSTOM)));
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Description text
    renderDescriptionText(guiGraphics, this.contentLeftPos + 5, this.contentTopPos + 60);
  }
}
