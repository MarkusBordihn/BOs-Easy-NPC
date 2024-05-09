/*
 * Copyright 2022 Markus Bordihn
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

package de.markusbordihn.easynpc.client.texture;

import de.markusbordihn.easynpc.Constants;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.TextureStitchEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@EventBusSubscriber(value = Dist.CLIENT, bus = EventBusSubscriber.Bus.MOD)
public class TextureStitcher {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private TextureStitcher() {}

  @SubscribeEvent
  public static void handleTextureStitchEvent(TextureStitchEvent.Pre event) {
    ResourceLocation stitching = event.getAtlas().location();
    if (!stitching.equals(InventoryMenu.BLOCK_ATLAS)) {
      return;
    }
    log.info("{} Texture to stitcher {}", Constants.LOG_REGISTER_PREFIX, InventoryMenu.BLOCK_ATLAS);

    // Adding empty armor slots icons.
    event.addSprite(
        new ResourceLocation(Constants.MOD_ID, "item/empty_armor/empty_armor_slot_boots"));
    event.addSprite(
        new ResourceLocation(Constants.MOD_ID, "item/empty_armor/empty_armor_slot_chestplate"));
    event.addSprite(
        new ResourceLocation(Constants.MOD_ID, "item/empty_armor/empty_armor_slot_helmet"));
    event.addSprite(
        new ResourceLocation(Constants.MOD_ID, "item/empty_armor/empty_armor_slot_leggings"));
    event.addSprite(
        new ResourceLocation(Constants.MOD_ID, "item/empty_armor/empty_armor_slot_shield"));
    event.addSprite(
        new ResourceLocation(Constants.MOD_ID, "item/empty_armor/empty_armor_slot_weapon"));
  }
}
