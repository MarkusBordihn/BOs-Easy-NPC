package de.markusbordihn.easynpc.network.message;

import de.markusbordihn.easynpc.Constants;
import java.util.Random;
import java.util.UUID;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.resources.ResourceLocation;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public interface NetworkMessageRecord extends CustomPacketPayload {

  Logger log = LogManager.getLogger(Constants.LOG_NAME);

  UUID EMPTY_UUID = new UUID(0L, 0L);

  Random RANDOM = new Random();

  ResourceLocation id();

  void write(FriendlyByteBuf friendlyByteBuf);
}
