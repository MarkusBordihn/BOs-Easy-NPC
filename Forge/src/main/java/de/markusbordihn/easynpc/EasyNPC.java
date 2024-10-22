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

import cpw.mods.modlauncher.Launcher;
import cpw.mods.modlauncher.api.IEnvironment;
import de.markusbordihn.easynpc.block.ModBlocks;
import de.markusbordihn.easynpc.client.model.ModModelLayer;
import de.markusbordihn.easynpc.client.renderer.ClientRenderer;
import de.markusbordihn.easynpc.client.renderer.manager.EntityTypeManager;
import de.markusbordihn.easynpc.client.screen.ClientScreens;
import de.markusbordihn.easynpc.commands.ModArgumentTypes;
import de.markusbordihn.easynpc.compat.CompatHandler;
import de.markusbordihn.easynpc.compat.CompatManager;
import de.markusbordihn.easynpc.config.Config;
import de.markusbordihn.easynpc.debug.DebugManager;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.io.DataFileHandler;
import de.markusbordihn.easynpc.item.ModItems;
import de.markusbordihn.easynpc.menu.MenuHandler;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import de.markusbordihn.easynpc.network.ClientNetworkMessageHandler;
import de.markusbordihn.easynpc.network.NetworkHandler;
import de.markusbordihn.easynpc.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.network.NetworkHandlerManagerType;
import de.markusbordihn.easynpc.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.network.ServerNetworkMessageHandler;
import de.markusbordihn.easynpc.network.syncher.EntityDataSerializersManager;
import java.util.Optional;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.fml.loading.FMLPaths;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod(Constants.MOD_ID)
public class EasyNPC {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public EasyNPC() {
    final IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();

    log.info("Initializing {} (Forge) ...", Constants.MOD_NAME);

    log.info("{} Debug Manager ...", Constants.LOG_REGISTER_PREFIX);
    Optional<String> version =
        Launcher.INSTANCE.environment().getProperty(IEnvironment.Keys.VERSION.get());
    if (version.isPresent() && "MOD_DEV".equals(version.get())) {
      DebugManager.setDevelopmentEnvironment(true);
    }
    DebugManager.checkForDebugLogging(Constants.LOG_NAME);

    log.info("{} Constants ...", Constants.LOG_REGISTER_PREFIX);
    Constants.GAME_DIR = FMLPaths.GAMEDIR.get();
    Constants.CONFIG_DIR = FMLPaths.CONFIGDIR.get();

    log.info("{} Configuration ...", Constants.LOG_REGISTER_PREFIX);
    Config.register();

    log.info("{} Entity Data Serializers ...", Constants.LOG_REGISTER_PREFIX);
    EntityDataSerializersManager.register();

    log.info("{} Compatibility Handler ...", Constants.LOG_REGISTER_PREFIX);
    CompatManager.registerCompatHandler(new CompatHandler());

    log.info("{} Command Argument Types ...", Constants.LOG_REGISTER_PREFIX);
    ModArgumentTypes.COMMAND_ARGUMENT_TYPES.register(modEventBus);

    log.info("{} Entity Types ...", Constants.LOG_REGISTER_PREFIX);
    ModEntityType.ENTITY_TYPES.register(modEventBus);

    log.info("{} Blocks ...", Constants.LOG_REGISTER_PREFIX);
    ModBlocks.BLOCKS.register(modEventBus);

    log.info("{} Blocks Entity Types ...", Constants.LOG_REGISTER_PREFIX);
    ModBlocks.BLOCK_ENTITY_TYPES.register(modEventBus);

    log.info("{} Items ...", Constants.LOG_REGISTER_PREFIX);
    ModItems.ITEMS.register(modEventBus);

    log.info("{} Menu Types ...", Constants.LOG_REGISTER_PREFIX);
    ModMenuTypes.MENU_TYPES.register(modEventBus);

    log.info("{} Menu Handler ...", Constants.LOG_REGISTER_PREFIX);
    MenuManager.registerMenuHandler(new MenuHandler());
    modEventBus.addListener(MenuHandler::registerMenuHandler);

    log.info("{} Network Handler ...", Constants.LOG_REGISTER_PREFIX);
    modEventBus.addListener(
        (final FMLCommonSetupEvent event) ->
            event.enqueueWork(
                () -> {
                  NetworkHandlerManager.registerHandler(new NetworkHandler());
                  NetworkHandlerManager.registerNetworkMessages(NetworkHandlerManagerType.BOTH);
                }));
    NetworkMessageHandlerManager.registerClientHandler(new ClientNetworkMessageHandler());

    DistExecutor.unsafeRunWhenOn(
        Dist.CLIENT,
        () ->
            () -> {
              log.info("{} Client events ...", Constants.LOG_REGISTER_PREFIX);
              modEventBus.addListener(ModModelLayer::registerEntityLayerDefinitions);
              modEventBus.addListener(ClientRenderer::registerEntityRenderers);
              modEventBus.addListener(ClientScreens::registerScreens);
              modEventBus.addListener(
                  (final FMLClientSetupEvent event) -> {
                    log.info("{} Register Data Files ...", Constants.LOG_REGISTER_PREFIX);
                    event.enqueueWork(DataFileHandler::registerDataFiles);

                    log.info("{} Register Entity Type Manager ...", Constants.LOG_REGISTER_PREFIX);
                    event.enqueueWork(EntityTypeManager::register);
                  });
              NetworkMessageHandlerManager.registerServerHandler(new ServerNetworkMessageHandler());
            });
  }
}
