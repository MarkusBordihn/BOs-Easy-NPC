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

package de.markusbordihn.easynpc;

import de.markusbordihn.easynpc.client.model.ModModelLayers;
import de.markusbordihn.easynpc.client.renderer.ClientRenderer;
import de.markusbordihn.easynpc.client.screen.ClientScreens;
import de.markusbordihn.easynpc.data.CustomDataHandler;
import de.markusbordihn.easynpc.data.CustomPresetData;
import de.markusbordihn.easynpc.data.CustomSkinData;
import de.markusbordihn.easynpc.entity.npc.ModEntityType;
import de.markusbordihn.easynpc.item.ModItems;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import de.markusbordihn.easynpc.network.NetworkHandler;
import de.markusbordihn.easynpc.tabs.EasyNPCTab;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod(Constants.MOD_ID)
public class EasyNPC {

  public static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public EasyNPC() {
    final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

    modEventBus.addListener(NetworkHandler::registerNetworkHandler);

    log.info("{} Entities Types ...", Constants.LOG_REGISTER_PREFIX);
    ModEntityType.ENTITY_TYPES.register(modEventBus);

    log.info("{} Items ...", Constants.LOG_REGISTER_PREFIX);
    ModItems.ITEMS.register(modEventBus);

    log.info("{} Menu Types ...", Constants.LOG_REGISTER_PREFIX);
    ModMenuTypes.MENU_TYPES.register(modEventBus);

    CustomDataHandler.prepare();

    DistExecutor.unsafeRunWhenOn(
        Dist.CLIENT,
        () ->
            () -> {
              log.info("{} Client events ...", Constants.LOG_REGISTER_PREFIX);
              modEventBus.addListener(ModModelLayers::registerEntityLayerDefinitions);
              modEventBus.addListener(ClientRenderer::registerEntityRenderers);
              modEventBus.addListener(ClientScreens::registerScreens);
              modEventBus.addListener(CustomPresetData::registerCustomPresetData);
              modEventBus.addListener(CustomSkinData::registerCustomSkinData);
              modEventBus.addListener(EasyNPCTab::handleCreativeModeTabRegister);
            });
  }
}
