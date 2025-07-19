package de.markusbordihn.easynpc.bundle;

import net.minecraftforge.fml.common.Mod;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Easy NPC Bundle Mod - Main Entry Point
 *
 * <p>This bundle mod automatically includes both the Easy NPC core mod and the Easy NPC Config UI
 * mod using Jar-in-Jar technology. Users only need to install this single file to get both mods.
 *
 * <p>The actual functionality is provided by the bundled mods: - easy_npc: Core functionality for
 * NPCs - easy_npc_config_ui: Configuration UI for NPCs
 */
@Mod(EasyNpcBundleMod.MOD_ID)
public class EasyNpcBundleMod {

  public static final String MOD_ID = "easy_npc_bundle";
  public static final String MOD_NAME = "Easy NPC Bundle";
  public static final String MOD_VERSION = "6.0.5";

  private static final Logger LOGGER = LogManager.getLogger();

  public EasyNpcBundleMod() {
    LOGGER.info("Initializing {} v{}", MOD_NAME, MOD_VERSION);
    LOGGER.info("This bundle automatically includes Easy NPC Core and Config UI mods");
  }
}
