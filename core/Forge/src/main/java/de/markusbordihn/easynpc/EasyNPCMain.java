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
import de.markusbordihn.easynpc.client.ClientEvents;
import de.markusbordihn.easynpc.client.model.ModModelLayer;
import de.markusbordihn.easynpc.client.renderer.BlockEntityRenderer;
import de.markusbordihn.easynpc.client.renderer.EntityRenderer;
import de.markusbordihn.easynpc.client.screen.ClientScreens;
import de.markusbordihn.easynpc.commands.ModArgumentTypes;
import de.markusbordihn.easynpc.commands.manager.CommandManager;
import de.markusbordihn.easynpc.compat.CompatHandler;
import de.markusbordihn.easynpc.compat.CompatManager;
import de.markusbordihn.easynpc.component.ModDataComponents;
import de.markusbordihn.easynpc.config.Config;
import de.markusbordihn.easynpc.debug.DebugManager;
import de.markusbordihn.easynpc.entity.LivingEntityEvents;
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
import de.markusbordihn.easynpc.server.ServerEvents;
import de.markusbordihn.easynpc.tabs.ModTabs;
import java.util.Optional;
import net.minecraft.world.entity.LivingEntity;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.ClientPlayerNetworkEvent;
import net.minecraftforge.client.event.EntityRenderersEvent;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.EntityAttributeCreationEvent;
import net.minecraftforge.event.entity.EntityJoinLevelEvent;
import net.minecraftforge.event.entity.EntityLeaveLevelEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.bus.BusGroup;
import net.minecraftforge.eventbus.api.listener.Priority;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.fml.loading.FMLEnvironment;
import net.minecraftforge.fml.loading.FMLPaths;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod(Constants.MOD_ID)
public class EasyNPCMain {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public EasyNPCMain(FMLJavaModLoadingContext context) {
    final BusGroup modBusGroup = context.getModBusGroup();

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
    Config.register(FMLEnvironment.dist == Dist.DEDICATED_SERVER);

    log.info("{} Common Data Files ...", Constants.LOG_REGISTER_PREFIX);
    DataFileHandler.registerCommonDataFiles();

    log.info("{} Entity Data Serializers ...", Constants.LOG_REGISTER_PREFIX);
    EntityDataSerializersManager.register();

    log.info("{} Compatibility Handler ...", Constants.LOG_REGISTER_PREFIX);
    CompatManager.registerCompatHandler(new CompatHandler());

    log.info("{} Command Argument Types ...", Constants.LOG_REGISTER_PREFIX);
    ModArgumentTypes.COMMAND_ARGUMENT_TYPES.register(modBusGroup);

    log.info("{} Entity Types ...", Constants.LOG_REGISTER_PREFIX);
    ModEntityType.ENTITY_TYPES.register(modBusGroup);

    log.info("{} Blocks ...", Constants.LOG_REGISTER_PREFIX);
    ModBlocks.BLOCKS.register(modBusGroup);

    log.info("{} Blocks Entity Types ...", Constants.LOG_REGISTER_PREFIX);
    ModBlocks.BLOCK_ENTITY_TYPES.register(modBusGroup);

    log.info("{} Items ...", Constants.LOG_REGISTER_PREFIX);
    ModItems.ITEMS.register(modBusGroup);

    log.info("{} Menu Types ...", Constants.LOG_REGISTER_PREFIX);
    ModMenuTypes.MENU_TYPES.register(modBusGroup);

    log.info("{} Menu Handler ...", Constants.LOG_REGISTER_PREFIX);
    MenuManager.registerMenuHandler(new MenuHandler());

    log.info("{} Mod Data Components ...", Constants.LOG_REGISTER_PREFIX);
    ModDataComponents.DATA_COMPONENTS.register(modBusGroup);

    log.info("{} Network Handler ...", Constants.LOG_REGISTER_PREFIX);
    NetworkMessageHandlerManager.registerClientHandler(new ClientNetworkMessageHandler());

    log.info("{} Creative Tabs ...", Constants.LOG_REGISTER_PREFIX);
    ModTabs.CREATIVE_TABS.register(modBusGroup);

    // Register MOD bus events
    FMLCommonSetupEvent.getBus(modBusGroup).addListener(this::commonSetup);
    EntityAttributeCreationEvent.BUS.addListener(ModEntityType::entityAttributeCreation);

    // Register CLIENT MOD bus events (only on client side)
    if (FMLEnvironment.dist == Dist.CLIENT) {
      registerClientEvents(modBusGroup);
    }

    // Register GAME bus events
    RegisterCommandsEvent.BUS.addListener(this::registerCommands);
    ServerStartingEvent.BUS.addListener(this::onServerStarting);
    TickEvent.ServerTickEvent.Post.BUS.addListener(this::onServerTick);
    EntityJoinLevelEvent.BUS.addListener(Priority.HIGHEST, this::onEntityJoinLevel);
    EntityLeaveLevelEvent.BUS.addListener(Priority.HIGHEST, this::onEntityLeaveLevel);

    // Register CLIENT GAME bus events (only on client side)
    if (FMLEnvironment.dist == Dist.CLIENT) {
      registerClientGameEvents();
    }
  }

  private void registerClientEvents(final BusGroup modBusGroup) {
    log.info("{} Client MOD bus events ...", Constants.LOG_REGISTER_PREFIX);
    FMLClientSetupEvent.getBus(modBusGroup).addListener(this::onClientSetup);
    EntityRenderersEvent.RegisterRenderers.BUS.addListener(this::onRegisterRenderers);
    EntityRenderersEvent.RegisterLayerDefinitions.BUS.addListener(this::onRegisterLayerDefinitions);
  }

  private void registerClientGameEvents() {
    log.info("{} Client GAME bus events ...", Constants.LOG_REGISTER_PREFIX);
    ClientPlayerNetworkEvent.LoggingOut.BUS.addListener(this::onPlayerLoggedOut);
    TickEvent.ClientTickEvent.Post.BUS.addListener(this::onClientTick);
  }

  private void commonSetup(final FMLCommonSetupEvent event) {
    event.enqueueWork(
        () -> {
          NetworkHandlerManager.registerHandler(new NetworkHandler());
          NetworkHandlerManager.registerNetworkMessages(NetworkHandlerManagerType.BOTH);
        });
  }

  private void onClientSetup(final FMLClientSetupEvent event) {
    ClientScreens.registerScreens(event);
    event.enqueueWork(
        () -> {
          ClientEvents.handleClientStartedEvent(net.minecraft.client.Minecraft.getInstance());
          NetworkMessageHandlerManager.registerServerHandler(new ServerNetworkMessageHandler());
        });
  }

  private void onRegisterRenderers(final EntityRenderersEvent.RegisterRenderers event) {
    EntityRenderer.register(event);
    BlockEntityRenderer.register(event);
  }

  private void onRegisterLayerDefinitions(
      final EntityRenderersEvent.RegisterLayerDefinitions event) {
    ModModelLayer.registerEntityLayerDefinitions(event);
  }

  private void onPlayerLoggedOut(final ClientPlayerNetworkEvent.LoggingOut event) {
    ClientEvents.handleWorldUnloadEvent();
  }

  private void registerCommands(final RegisterCommandsEvent event) {
    CommandManager.registerCommands(event.getDispatcher(), event.getBuildContext());
  }

  private void onServerStarting(final ServerStartingEvent event) {
    ServerEvents.handleServerStarting(event.getServer());
  }

  private void onServerTick(final TickEvent.ServerTickEvent event) {
    ServerEvents.handleServerTick(event.server());
  }

  private void onClientTick(final TickEvent.ClientTickEvent event) {
    ClientEvents.handleClientTickEvent();
  }

  private void onEntityJoinLevel(final EntityJoinLevelEvent event) {
    if (event.getEntity() instanceof LivingEntity livingEntity) {
      LivingEntityEvents.handleLivingEntityJoinEvent(livingEntity);
    }
  }

  private void onEntityLeaveLevel(final EntityLeaveLevelEvent event) {
    if (event.getEntity() instanceof LivingEntity livingEntity) {
      LivingEntityEvents.handleLivingEntityLeaveEvent(livingEntity);
    }
  }
}
